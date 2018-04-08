{-# LANGUAGE TupleSections #-}
module Lambda.Strategy where
import Lambda.Term
import Lambda.Read (paren, (&&&), mapP, sym)
import Lambda.Nav
import Data.Proxy
import Data.Maybe (catMaybes, maybeToList)
import Data.List ((\\), intersect)

-- We associate different reduction strategies with a redex type
class Redex r where
  redexes   :: Expr -> [r]           -- return set of possible redexes in an Expr
  reduce    :: r -> Expr -> Expr     -- reduce a given redex into a different Expr
  redexDesc :: r -> String           -- description of the type of a redex
  hasPath   :: r -> Nav -> Bool      -- whether a redex contains a navigation path 

-- A default redex implementation that provides all possible alpha/beta redexes
data BasicRedex = BasicRedex 
  { basicRedexPath :: Nav 
  , basicRedexType :: BasicRedexType
  } deriving Eq

data BasicRedexType = Alpha | Beta deriving (Eq, Enum, Show, Read)

instance Read BasicRedex where
  readsPrec _ = mapP (uncurry BasicRedex) $ reads &&& paren reads

instance Show BasicRedex where
  show (BasicRedex nv t) = show nv ++ "(" ++ show t ++ ")"

instance Redex BasicRedex where
  redexes e = map (navToBasic e) $ allRedexNavs e 
  reduce (BasicRedex nv _) e = fromZipper (either id id $ alphaOrBeta e', f)
    where (e', f) = navigate nv e
  redexDesc =  show . basicRedexType
  hasPath = (==) . basicRedexPath 

navToBasic e nv = BasicRedex nv $ either (const Alpha) (const Beta) $ alphaOrBeta e'
  where (e', _) = navigate nv e

-- A left-most outer-most redex used by normal order reduction
newtype LeftOuterRedex = LeftOuterRedex BasicRedex deriving Eq

instance Read LeftOuterRedex where
  readsPrec p = mapP LeftOuterRedex (readsPrec p)

instance Show LeftOuterRedex where
  show (LeftOuterRedex r) = show r

instance Redex LeftOuterRedex where
  redexes = head' . map LeftOuterRedex . redexes 
    where head' (x:_) = [x]
          head' _ = []
  reduce (LeftOuterRedex r) = reduce r
  redexDesc (LeftOuterRedex r) = redexDesc r
  hasPath (LeftOuterRedex r) = (==) $ basicRedexPath r

-- A left-most inner-most redex used by applicative order reduction
newtype LeftInnerRedex = LeftInnerRedex BasicRedex deriving Eq

instance Read LeftInnerRedex where
  readsPrec p = mapP LeftInnerRedex (readsPrec p)

instance Show LeftInnerRedex where
  show (LeftInnerRedex r) = show r

instance Redex LeftInnerRedex where
  redexes e = (map (LeftInnerRedex . navToBasic e) . maybeToList . leftInner) e
    where
      leftInner :: Expr -> Maybe Nav
      leftInner = afold aux . toNavAExpr
        where
          x@(Just _) `before` _ = x
          _ `before` y = y
          afold f = ffix (\h (AExpr ns t) -> f ns t (h t))
          aux _ _ (Lam _ e) = e
          aux ns x (App f e) = f `before` e `before` if isRedex x then Just ns else Nothing
            where
              isRedex (App (AExpr _ (Lam _ _)) _) = True
              isRedex _ = False
          aux _ _ _ = Nothing
  reduce (LeftInnerRedex r) = reduce r
  redexDesc (LeftInnerRedex r) = redexDesc r
  hasPath (LeftInnerRedex r) = (==) $ basicRedexPath r

-- A left-most outer-most redex that is not under a lambda, as used
-- by call-by-name evaluation.
newtype CBNRedex = CBNRedex BasicRedex deriving Eq

instance Read CBNRedex where
  readsPrec p = mapP CBNRedex (readsPrec p)

instance Show CBNRedex where
  show (CBNRedex r) = show r

instance Redex CBNRedex where
  redexes e = (map (CBNRedex . navToBasic e) . maybeToList . cbn) e
    where
      cbn :: Expr -> Maybe Nav
      cbn = afold aux . toNavAExpr
        where
          x@(Just _) `before` _ = x
          _ `before` y = y
          afold f = ffix (\h (AExpr ns t) -> f ns t (h t))
          aux ns x (App f e) = (if isRedex x then Just ns else Nothing) `before`
                               f `before` e 
            where
              isRedex (App (AExpr _ (Lam _ _)) _) = True
              isRedex _ = False
          aux _ _ _ = Nothing
  reduce (CBNRedex r) = reduce r
  redexDesc (CBNRedex r) = redexDesc r
  hasPath (CBNRedex r) = (==) $ basicRedexPath r

-- A left-most inner-most redex that is not under a lambda, as used
-- by call-by-value evaluation. 
newtype CBVRedex = CBVRedex BasicRedex deriving Eq

instance Read CBVRedex where
  readsPrec p = mapP CBVRedex (readsPrec p)

instance Show CBVRedex where
  show (CBVRedex r) = show r

instance Redex CBVRedex where
  redexes e = (map (CBVRedex . navToBasic e) . maybeToList . cbv) e
    where
      cbv :: Expr -> Maybe Nav
      cbv = afold aux . toNavAExpr
        where
          x@(Just _) `before` _ = x
          _ `before` y = y
          afold f = ffix (\h (AExpr ns t) -> f ns t (h t))
          aux ns x (App f e) = f `before` e `before` if isRedex x then Just ns else Nothing
            where
              isRedex (App (AExpr _ (Lam _ _)) _) = True
              isRedex _ = False
          aux _ _ _ = Nothing
  reduce (CBVRedex r) = reduce r
  redexDesc (CBVRedex r) = redexDesc r
  hasPath (CBVRedex r) = (==) $ basicRedexPath r

-- Standard reduction is to reduce the first redex returned from redexes
standard :: Redex r => Proxy r -> Expr -> Maybe Expr
standard p e = if null rs then Nothing else Just (reduce (head rs `asProxyTypeOf` p) e)
  where rs = redexes e 

-- Return navigation paths to all possible redexes
allRedexNavs :: Expr -> [Nav]
allRedexNavs = map Nav . ffix (\h x -> aux x $ h $ unExpr x)
  where
    aux _ (Lam _ e) = map (D:) e
    aux x (App f e) = whenRedex x $ map (L:) f ++ map (R:) e
      where
        whenRedex (Expr (App (Expr (Lam _ _)) _)) = ([]:)
        whenRedex _ = id
    aux _ _ = []

alphaOrBeta (Expr (App (Expr (Lam v e)) e')) 
            | freeVars e' `intersect` boundVars e == [] = Right $ subst v e' e
            | otherwise = Left $ Expr (App (Expr (Lam v (alpha (freeVars e') e))) e')
alphaOrBeta _ = error "expect a valid beta redex"

-- return a new expr that is alpha equivalent to e, but with no bound variables that 
-- appear in fvs.
-- Note that in choosing a replacement binding variable, it should not accidentally 
-- capture free vars under this sub-expression.
alpha fvs = fold aux
  where
    aux (Lam v e') | v `elem` fvs = Expr (Lam u (subst v (Expr (Var u)) e')) 
      where (u:_) = [ w | w <- vars (freeVars e'), w `notElem` boundVars e' ] 
    aux e = Expr e
    vars fvs' = [ v | v <- full, v `notElem` fvs, v `notElem` fvs' ] 

full' atoz = map V (map (:[]) atoz ++ [ v : show m | v <- atoz, m <- [0..]])
full = full' ['a' .. 'z']

-- check if two expressions are alpha equivalent
alphaEq (Expr e) (Expr e') = aux e e'
  where
    aux (Lam v e) (Lam u e') | v == u    = alphaEq e e'
                             | otherwise = alphaEq (subst v w e) (subst u w e')
                                             where (w:_) = [ Expr (Var w) 
                                                           | w <- full
                                                           , w `notElem` freeVars e
                                                           , w `notElem` freeVars e'
                                                           , w `notElem` boundVars e
                                                           , w `notElem` boundVars e' 
                                                           ]
    aux (App f e) (App f' e') = alphaEq f f' && alphaEq e e'
    aux e e' = e == e'

-- Test properties of alphaEq with the help of allLamNavs
propAlpha :: Int -> Int -> Expr -> Bool
propAlpha i j e = null ns || (alphaEq e e1 && alphaEq e1 e) && 
                    (null fvs || not (alphaEq e1 e2) && not (alphaEq e2 e1) &&
                                 not (alphaEq e1 e3) && not (alphaEq e3 e1)) &&
                    (null fvs' || not (alphaEq e4 e4' && not (alphaEq e4' e4)) &&
                                  ((alphaEq e5 e5') || error (show (e5, e5'))))
  where
    ns = allLamNavs e 
    nav = ns !! (abs i `mod` length ns)
    (Expr (Lam v e'), ctx) = navigate nav e
    bvs = boundVars e'
    fvs = filter (/=v) $ freeVars e' 
    -- a case that is always valid
    u1 = head [ u | u <- full, u `notElem` bvs, u `notElem` fvs ]
    e1 = fromZipper (Expr $ Lam u1 (subst v (Expr $ Var u1) e'), ctx)
    -- a case that is always invalid
    u2 = fvs !! (abs j `mod` length fvs)
    e2 = fromZipper (Expr $ Lam u2 (subst v (Expr $ Var u2) e'), ctx)
    -- a case that is always invalid
    u3 = fvs !! (abs j `mod` length fvs)
    e3 = fromZipper (Expr $ Lam v (subst u3 (Expr $ Var v) e'), ctx)
    -- a case that is always invalid
    fvs' = fvs `intersect` freeVars e 
    u4 = fvs' !! (abs j `mod` length fvs')
    e4  = Expr $ Lam u4 e
    e4' = Expr $ Lam v $ fromZipper (Expr $ Lam v (subst u4 (Expr $ Var v) e'), ctx)
    -- a case that is always valid
    (u5:u6:_) = [ u | u <- full, u `notElem` freeVars e, u `notElem` boundVars e, u `notElem` fvs, v `notElem` fvs ]
    e5  = Expr $ Lam u4 e
    e5' = Expr $ Lam u5 $ subst u4 (Expr $ Var u5) $ fromZipper (Expr $ Lam u6 (subst v (Expr $ Var u6) e') , ctx)


