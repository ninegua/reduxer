-- A CGI program for Lambda Calculator.
{-# LANGUAGE GADTs, TupleSections #-}
module Main where
import Lambda
--import Lambda.Strategy
import qualified Control.Exception as E 
import Control.Arrow (first)
import Control.Applicative ((<$>),(<*>))
import Network.CGI
import Network.CGI.Protocol
import System.IO
import System.Environment (getEnv)
import System.Process (runInteractiveCommand, waitForProcess)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as LU 
import Data.Proxy
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Text.XHtml
import Test.QuickCheck 
import Numeric (readDec)
import Data.List (mapAccumL, transpose, delete, union)
import Lambda
import Lambda.Parse
import DotGraph

-- Render some text that reacts to mouseover and click events.
-- Because <a> tag can't be nested, we use <span> instead.
clickableSpan ns s = 
  (thespan << s) !
    [ theclass "normal",
      strAttr "onclick"     $ "(function(e){reduce('" ++ i ++ "', e);}) ()",
      strAttr "onmouseover" $ "(function(e){redexOver('" ++ i ++ "',e);}) ()",
      strAttr "onmouseout"  $ "(function(e){redexOut('" ++ i ++ "',e);}) ()",
      name i]
  where i = show ns

-- Render an expression (among many in a reduction sequence): 
-- 1. all redexes can react to user actions;
-- 2. the URL for each redex is dictated by by a sequence of navigation paths
--    that represent the reduction sequence, hence the prefix parameter;
-- 3. the redex that leads to the next reduction will be decorated after checking
--    against the Nav path.
toClickable :: (Nav->Html->Html) -> Expr -> Html
toClickable decorate = snd . afold aux . toNavAExpr
  where
    afold f = ffix (\h (AExpr ns t) -> f ns t (h t))
    aux ns _ (Var (V v)) = (either id id, decorate ns $ stringToHtml v)
    aux ns _ (Lam (V v) (_,e)) = (either pr pr, decorate ns $ "λ" +++ v +++ "." +++ e)
    aux ns t (App (b,f) (d,e)) = (either id pr, decorate ns $ b (Left f) +++ " " +++ d (Right e))
    pr s = "(" +++ s +++ ")"

--Render some lines as a visual cue to indicate the nesting of redexes.
toLines :: Redex r => (Nav -> Maybe r) -> Expr -> [String]
toLines isRedex = draw . snd . afold aux . toNavAExpr
  where
    afold f = ffix (\h (AExpr ns t) -> f ns t (h t))
    aux ns _ (Var (V v)) = (either id id, sp (length v))
    aux ns _ (Lam (V v) (_,e)) = (either pr pr, sp (length v + 2) ++ e)
    aux ns t (App (b,f) (d,e)) = (either id pr, incRedex s)
      where
        s = b (Left f) ++ [0] ++ d (Right e)
        incRedex = maybe id (\_ -> map (+1)) (isRedex ns) 
    pr s = [0] ++ s ++ [0]
    sp i = take i $ repeat 0
    draw s = transpose $ map (\i -> take (m-i) (repeat ' ') ++ take i (repeat '_')) s
      where m = maximum s

-- toScript :: [String] -> (Nav -> Bool) -> [Nav] -> Expr -> Html
toScript
  :: Redex r =>
     (Expr -> Nav -> Maybe r)
     -> ((Nav -> Maybe r) -> Nav -> Html -> Html) -> Expr -> Html
toScript isRedexOf decorate exp =
  thediv ! [theclass "front mono", identifier "exp"] <<
  toClickable (decorate isRedex) exp +++ thediv ! [theclass "back mono"] <<
  unlines (toLines isRedex exp)
  where
    isRedex = isRedexOf exp

--Eventually, we want to run the dot command from the system, render
--the graph as a PNG, and read it back as binary data.

toPNG :: (String -> IO a) -> Expr -> IO L.ByteString
toPNG err e = do
  let input = toDot e
      handler :: E.SomeException -> IO B.ByteString
      handler e = err "Broken Pipe: perhaps your path to dot is wrong?\n" >> return B.empty
  -- Use dot to layout nodes, use neato to draw edges
  (inp, out, _, proc) <- runInteractiveCommand ("export PATH=/run/current-system/sw/bin:$PATH;dot|neato -n1 -Gsplines=true -Tpng")
  s <- E.catch (do
         B.hPutStrLn inp (BU.fromString input)
         B.hPutStrLn inp B.empty
         hClose inp
         err "finished writing input to dot\n"
         B.hGetContents out) handler
  err $ "PNG byte length = " ++ show (B.length s) ++ "\n"
  exitcode <- waitForProcess proc
  err $ show exitcode ++ "\n"
  return $ L.fromChunks [s]

--Finally, we run the entire process as a CGI program.

withHtmlHeader html = setContentType "text/html; charset=utf-8" >> outputFPS (LU.fromString $ renderHtml html)
withPNGHeader s = setContentType "image/png" >> outputFPS s
setContentType = setHeader "Content-Type"

inputForm method str = thediv << 
  [ paragraph << "Click to reduce, both beta and alpha (if needed) steps will be shown."
  , ulist << map (li <<) introtext
  , p << ("Here are a few examples you can try:")
  , olist << map (\x -> li << (anchor << x) ! [ href $ "lambda?t=" ++ x ]) examples
  , (form << 
      [ textfield "term" ! [ theclass "mono", value str ]
      , (select << map mkOption strategies) --submit "" "Submit" 
        ! [ name "method", strAttr "onchange" $ "return reduce('',0);" ]
      ]) ! [ name "entry", strAttr "onsubmit" $ "return reduce('',0);" ]
  ]
  where
    strategies = [ "any order", "applicative order", "normal order", 
                   "call-by-value", "call-by-name", "call-by-need", "ariola-felleisen" ]
    mkOption v = (option << v) ! (value v : if v == method then [selected] else [])

isRedex' :: Redex r => [r] -> Expr -> Nav -> Maybe r
isRedex' prefix exp x = listToMaybe $ filter (\ns -> hasPath ns x) $ redexes exp 

sections :: (Redex r, Show r) => Expr -> [r] -> (Html, Expr)
sections exp ns = (sec +++ toScript isRedex (clickable prefix') exp', exp')
  where
    ((exp', prefix'), sec) = mapAccumL section (exp, []) ns
    isRedex = isRedex' ns -- here ns is only used to enforce type of r
    clickable prefix isRedex = maybe id (clickableSpan . (prefix++) . (:[])) . isRedex
    section (exp, prefix) ns = ((exp', prefix'), toScript isRedex decorate exp)
      where 
        prefix' = prefix ++ [ns]
        label = redexDesc ns 
        exp' = reduce ns exp
        -- decorate :: Nav -> Html -> Html
        decorate isRedex x = (if hasPath ns x then darken else id) . 
                             clickable prefix isRedex x
        darken x = thespan ! [ theclass label ] << thespan ! [ theclass "darken" ] << x

pageHeader method exprstr content = 
  header << (thetitle << "Lambda Viewer" +++ sty +++ js) +++ body << 
  ((h3 << "Lambda Viewer") +++ inputForm method exprstr ! [ theclass "block" ] +++ content)
  where
    sty = style << primHtml stylesheet
    js  = (script << primHtml javascript) ! [ strAttr "language" "JavaScript"]

page :: (Show r, Redex r) => String -> [r] -> Expr -> Html
page method rds exp = pageHeader method (show exp) $ thediv << (sec +++ img)
  where 
    (sec, exp') = sections exp rds 
    img = image ! [src $ "lambda?i=1&t=" ++ (show exp')]

trimHead = dropWhile (==' ')
trim = trimHead . reverse . trimHead . reverse

cgiMain = do
  t <- getInputFPS "t"
  r <- getInput "r"
  i <- getInput "i"
  m <- fmap (maybe "" id) $ getInput "m"
  let readRedexes :: Read a => Maybe String -> [a]
      readRedexes = maybe [] read'
      read' :: Read a => String -> [a]
      read' x = case reads x of
                  [(r,"")] -> r
                  _ -> []
  case t of
    Nothing -> withHtmlHeader $ pageHeader "" "" ""
    Just s -> do
      let term = trim $ LU.toString s
      case parseTerm $ term of
        Left  err -> withHtmlHeader $ pageHeader "" term $ stringToHtml $ show err
        Right exp -> maybe htmlRes imageRes i
          where htmlRes = withHtmlHeader $ 
                    case m of
                      "ariola-felleisen" -> page m (readRedexes r :: [AFRedex]) exp
                      "call-by-need" -> page m (readRedexes r :: [CbnRedex]) exp
                      "call-by-value" -> page m (readRedexes r :: [CBVRedex]) exp
                      "call-by-name" -> page m (readRedexes r :: [CBNRedex]) exp
                      "applicative order" -> page m (readRedexes r :: [LeftInnerRedex]) exp
                      "normal order" -> page m (readRedexes r :: [LeftOuterRedex]) exp
                      _ -> page m (readRedexes r :: [BasicRedex]) exp
                imageRes _ = liftIO (toPNG (writeFile "/tmp/lambda.log") exp) >>= withPNGHeader 

main :: IO ()
main = runCGI $ handleErrors cgiMain

-- The rest are the introduction text, examples, style sheet and java script.

introtext  =
  [ "You may use \\ for the λ symbol, and ( and ) to group lambda terms."
  , "A space is required to denote application."
  , "The scope of abstraction extends to the rightmost."
  , "Application is left associative."
  ]
examples   =
  [ "(λx.λy.y) ((λx.x x) (λx.x x))"
  , "(λf.f (f x)) ((λx.λy.x) (λx.y))"
  , "(λf.λx.f (f x)) (λf.λx.f (f x)) (λx.x) (λx.x)"
  ]
stylesheet = concat
  [ "#term { width : 30em; }"
  , "#exp { color: blue; }"
  , ".normal { color: blue; cursor: pointer; }"
  , ".bright { color: red; cursor: pointer; }"
  , ".darken { background-color: #eeeeee; }"
  , ".mono { font-size: 11pt; font-family: monospace; }"
  , ".front { position: relative; z-index: 2; margin-top: 1em;"
  ,         " white-space: pre; }"
  , ".back  { position: relative; top: -6pt; color: lightgrey;"
  ,         " z-index: 1; white-space: pre; line-height: 3px; }"
  , ".Alpha:after { content:\"α\"; color:lightgrey; margin-right: -0.6em; position: relative; top:-1em; font-size:80%; }"
  , ".GC:after { content:\"GC\"; color:lightgrey; margin-right: -0.6em; position: relative; top:-1em; font-size:80%; }"
  , ".Assoc:after { content:\"A\"; color:lightgrey; margin-right: -0.6em; position: relative; top:-1em; font-size:80%; }"
  , ".Lift:after { content:\"L\"; color:lightgrey; margin-right: -0.6em; position: relative; top:-1em; font-size:80%; }"
  , ".Deref:after { content:\"D\"; color:lightgrey; margin-right: -0.6em; position: relative; top:-1em; font-size:80%; }"
  , ".Beta:after { content:\"β\"; color:lightgrey; margin-right: -0.6em; position: relative; top:-1em; font-size:80%; }"
  , "html { display: table; margin: auto; }"
  , "body { display: table-cell; vertical-align: middle; }"
  , "ul,ol { padding-left: 1em; }"
  , ".block { width: 800px; }"
  , "img { margin-top:1em; max-width: 750px; }"
  ]
javascript = concat
  [ "function redex(id, name) { var i, j, a;"
  ,   "id = id.substring(0, id.lastIndexOf('('));"
  ,   "for (i = 0; (a = document.getElementsByTagName('span')[i]); i++) {"
  ,     "aname = a.getAttribute('name');"
  ,     "j = aname ? aname.lastIndexOf('(') : -1;"
  ,     "if (j>0) {"
  ,       "aname = aname.substring(0,j);"
  ,       "if (aname.indexOf(id) == 0 && aname.indexOf('(', id.length) < 0) {"
  ,         "a.className=name;"
  ,       "}"
  ,     "}"
  ,   "}"
  , "}"
  , "function encode(s) { return s; }"
  , "function stopBubble(e) {if (e && e.stopPropagation) { e.stopPropagation(); } else { window.event.cancelBubble = true;}}"
  , "function reduce(id,e) {"
  ,   "a=document.forms['entry'];"
  ,   "m=a.elements['method'];"
  ,   "window.location='lambda?t=' + encode(a.elements['term'].value) + "
  ,      "'&r=' + id + '&m=' + m.options[m.selectedIndex].value;"
  ,   "stopBubble(e);"
  ,   "return false;"
  , "}"
  , "function redexOver(id,e) { redex(id,'bright');stopBubble(e); }"
  , "function redexOut(id,e)  { redex(id,'normal');stopBubble(e); }"
  ]


