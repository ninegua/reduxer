module Lambda.CSF where

import Lambda.Term
import Lambda.Nav
import Lambda.Show
import Lambda.Read ((&&&), mapP, sym, paren)
import Lambda.Strategy
import Lambda.Test
import Data.Monoid
import Data.List (intersect, nub)

-- We implement the call-by-need calculus as laid out in [1]
--
-- [1] Chang, Stephen, and Matthias Felleisen. "The call-by-need lambda calculus, revisited." 
--     Proceedings of the 21st European conference on Programming Languages and Systems. 
--     Springer-Verlag, 2012.
--
-- The algorithm decomposes a term into a set of environments, which helps locating the
-- exact place that absolutely needs a beta substitution. 

-- A Context with one (or possibly multi-instances of the same) hole can be seen as a 
-- function: 
--    Ctx = Expr -> Expr
-- If restricted to single hole, Context is a Nav to the sub-expression it encloses.
type Ctx = Nav 

-- Each type of context has a parser-like function that map e to (e', nv) pair, where
-- e' is the sub-expression, and nv represents the context. 
type CtxP = Expr -> [ (Expr, Ctx) ]

-- The property of p :: CtxP is that the returned navigation should indeed lead to 
-- the returned sub-expression.
propCtxP :: CtxP -> Expr -> Bool
propCtxP p exp = all id $ do
    (e, nv) <- p exp
    return $ fst (navigate nv exp) == e

-- The empty CtxP []
hole :: CtxP
hole e = [(e,mempty)]

-- The union of two CtxP
(<|>) :: CtxP -> CtxP -> CtxP
p <|> q = \e -> p e ++ q e

-- Answer: a = A[v] 
-- Answer Context: A = [] | A[\x.A] e
-- Real meaning: contexts in which all applications are matched up.
ctxA :: CtxP
ctxA = hole <|> ctxA'

ctxA' (Expr (App f e)) = 
    [ (e2, left <> p1 <> down <> p2)
    | ((Expr (Lam x e1)), p1) <- ctxA f
    , (e2, p2) <- ctxA e1
    ]
ctxA' _ = []

-- Partial Answer Context Outer: A^ = [] | A[A^] e.
-- Real meaning: answer contexts of possible applications.
ctxO :: CtxP
ctxO = hole <|> ctxO' -- (filter (isApp . fst) . ctxO')
  where isApp (Expr (App _ _)) = True
        isApp _ = False

ctxO' (Expr (App f e)) = 
    [ (e2, left <> p1 <> p2) 
    | (e1, p1) <- ctxA f
    , (e2, p2) <- ctxO e1
    ]
ctxO' _ = []

-- Partial Answer Context Inner: Av = [] | A[\x.Av]
-- Real meaning: answer contexts of possible abstractions, such that when combined with the
-- corresponding A^, A^[Av] forms a real answer context.
ctxI :: CtxP
ctxI = hole <|> ctxI' -- (filter (isLam . fst) . ctxI')
  where isLam (Expr (Lam _ _)) = True
        isLam _ = False

ctxI' e = 
    [ (e2, p1 <> down <> p2)
    | ((Expr (Lam v e1)), p1) <- ctxA e
    , (e2, p2) <- ctxI e1
    ]

-- Evaluation Context: E = [] | E e | A[E] | A^[A[\x.Av[E[x]]] E'], where A^[Av] also forms A
-- Real meaning: E[x] means x is demanded, i.e., evaluating E[x] will first require evaluating x.
ctxE :: CtxP
ctxE = nub . (hole <|> ctxE1 <|> ctxE2 <|> ctxE3)

ctxE1 :: CtxP
ctxE1 (Expr (App f e)) = [ (e1, left <> p) | (e1, p) <- ctxE f ]
ctxE1 _ = []

ctxE2 :: CtxP
ctxE2 e = [ (e2, p1 <> p2) | (e1, p1) <- ctxA e, e1 /= e, (e2, p2) <- ctxE e1 ]

ctxE3 :: CtxP
ctxE3 = fmap (\(RedexParts _ arg po pa pi pe parg)  -> (arg, po <> right <> parg)) . ctxE3'

data RedexParts = RedexParts { expr :: Expr, arg :: Expr, po :: Nav, pa :: Nav, pi :: Nav, pe :: Nav, parg :: Nav }

ctxE3' e = unique equiv
    [ RedexParts e e5 p1 p2 p3 p4 p5 
    | (Expr (App f1 e1), p1) <- ctxO e
    , (Expr (Lam v e2), p2) <- ctxA f1
    , (e3, p3) <- ctxI e2
    -- , notCaptured v e2 p3
    , (Expr (Var x), p4) <- ctxE e3
    , x == v 
    , (e5, p5) <- ctxE e1
    , wellformed (navigate p1 e) (navigate p3 e2)
    ]
  where
    notCaptured v e2 p3 = elem v $ freeVars (fromZipper (Expr (Var v), snd (navigate p3 e2)))
    wellformed (e_o, z_o) (e_i, z_i) = any (check . fst) $ ctxA e
      where 
        e = fromZipper (bar, z_i ++ z_o)
        check e = e == bar

-- A^[A1[\x.Av[E[x]]] A2[\z.E'[z]]] ==> A^[A1[A2[Av[E[x]]{x:=\z.E'[z]]]]
-- A^[A1[\x.Av[E[x]]] A2[v]] ==> A^[A1[A2[Av[E[x]]{x:=v}]]]
checkBeta e = 
  case filter (/=Nothing) $ map betas $ unique equiv' $ ctxE3' e of
    [x] -> x
    [] -> Nothing
    r -> error $ "expect unique match, but got " ++ show r
  where
    betas (RedexParts _ _ po pa pi pe parg) = 
      let (Expr (App e1 arg), f1) = navigate po e
          fvs = freeVars arg
          isValue (Expr (Lam _ _)) = True
          isValue (Expr (Var x)) = False -- elem x fvs
          isValue _ = False
      in case filter (isValue . fst) (ctxA arg) of
        [(v, pv)] -> Just $ if needsAlpha then Left rdx else Right rdx
          where
            rdx = (po, pa, pv)
            (Expr (Lam x e2), f2) = navigate pa e1
            -- check if A2 happens to capture free variables in e2
            fvs2 = freeVars e2
            a2 = fromZipper (bar, snd $ navigate pv arg)
            -- check if A1 happens to capture free variables in v
            needsAlpha :: Bool
            fvsV = freeVars v
            needsAlpha = (not $ null $ fvs2 `intersect` boundVars a2) ||
                         (not $ null $ fvsV `intersect` boundVars e1)
        [] -> Nothing
        r -> error $ "expect unique match of " ++ show arg ++ " with A[v], but got " ++ show r
     
betaReduce e = 
  case checkBeta e of
    Nothing -> error $ "beta redex not found"
    Just rdx -> either (f True) (f False) rdx
      where 
        f needsAlpha (po, pa, pv) = if needsAlpha then e' else e''
          where
            -- if captured, we alpha convert arg and e1
            (Expr (App e1 arg), f1) = navigate po e
            (Expr (Lam x e2), f2) = navigate pa e1
            fvs2 = freeVars e2
            arg' = alpha fvs2 arg
            (v, _) = navigate pv arg'
            fvsV = freeVars v
            e1' = alpha fvsV e1
            e' = fromZipper (Expr (App e1' arg'), snd $ navigate po e)
            -- otherwise e'' = A^[A1[A2[Av[E[x]]{x:=v}]]]
            (w, _) = navigate pv arg
            e2' = subst x w e2
            e'' = fromZipper (fromZipper (fromZipper (e2', snd $ navigate pv arg), f2), f1)

unique equiv (x:xs) = x : unique equiv (filter (not . equiv x) xs)
unique equiv [] = []

-- this is used to consolidate matches for A^[A1[\x.Av[E[x]]] A2[v]], where A2[v] is treated as a single Expr
equiv' (RedexParts e arg po pa pi _ _) (RedexParts e' arg' qo qa qi _ _) = [po,pa,pi] == [qo,qa,qi] && e == e' 
-- this is used to consolidate matches for A^[A1[\x.Av[E[x]]] E], where E is a set of parse results.
equiv (RedexParts e arg po pa pi _ _) (RedexParts e' arg' qo qa qi _ _) = [po,pa,pi] == [qo,qa,qi] && e == e' && arg == arg'

instance Show RedexParts where
  show (RedexParts e arg po pa pi pe parg) = unlines 
      [ "Parse " ++ show e ++ " in the form of A^[A[\955X.Av[E[X]]] E']:" 
      , "  A^ = " ++ show ( fromZipper (bar, f1))
      , "  A  = " ++ show (fromZipper (bar, f2))
      , "  X  = " ++ show x
      , "  Av = " ++ show ( fromZipper (bar, f3))
      , "  E  = " ++ show e3
      , "  E' = " ++ show e'
      , "  arg = " ++ show arg
      ]
    where
       (Expr (App e1 e'), f1) = navigate po e
       (Expr (Lam x e2), f2) = navigate pa e1
       (e3, f3) = navigate pi e2
       (e4, f4) = navigate pe e3
       (e5, f5) = navigate parg e'

barV = V "_"
bar = Expr (Var barV)

-- Standard redex in call-by-need. E[e] ==> E[e'], where e => e' is a beta reduction.
data CSFRedex = CSFRedex BasicRedex (Nav, Nav, Nav) deriving Eq

instance Read CSFRedex where
  readsPrec _ = mapP f $ readNav &&& readNav &&& readNav &&& reads &&& paren reads
    where
      readNav = mapP fst $ reads &&& sym ':'
      f ((((ctx,po),pa),pv),t) = CSFRedex (BasicRedex ctx t) (po,pa,pv)

instance Show CSFRedex where
  show (CSFRedex (BasicRedex ctx t) (po, pa, pv)) = 
    show ctx ++ ":" ++ show po ++ ":" ++ show pa ++ ":" ++ show pv ++ "(" ++ show t ++ ")"

instance Redex CSFRedex where
  redexes e = concatMap toRedex $ ctxE e 
    where 
      toRedex (e, ctx) = maybe [] (\x -> 
        [CSFRedex (BasicRedex ctx (either (\_ -> Alpha) (\_ -> Beta) x)) (either id id x)]) $ 
          checkBeta e
  reduce (CSFRedex (BasicRedex ctx _) _) e = fromZipper (betaReduce e', f)
    where (e', f) = navigate ctx e
  redexDesc (CSFRedex r _) = redexDesc r
  hasPath (CSFRedex (BasicRedex ctx _) (po, pa, pv)) ns = p == ns || q == ns
    where
      p = ctx <> po <> left <> pa
      q = ctx <> po <> right <> pv

-- standard reduction is uniq
propUniq e = length (redexes e :: [CSFRedex]) <= 1

