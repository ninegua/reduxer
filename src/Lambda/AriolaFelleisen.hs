{-# LANGUAGE TupleSections #-}
module Lambda.AriolaFelleisen where

import Lambda.Term
import Lambda.Nav
import Lambda.Show
import Lambda.Read ((&&&), mapP, paren)
import Lambda.Strategy hiding (BasicRedexType(..))
import Lambda.Test
import Data.Monoid
import Data.Maybe 
import Control.Arrow (first)
import Data.List (intersect, nub)
import Lambda.CSF (Ctx, CtxP, propCtxP, hole, (<|>))

-- We implement the call-by-need calculus as laid out in [1]
--
-- [1] Ariola, Z.M., Felleisen, M.: The call-by-need lambda calculus. J. Funct. Program. 
--     7, 265-301 (1997)

-- E = [] | E e | (\x.E[x]) E | (\x.E) e
ctxE = nub . (hole <|> ctxE1 <|> ctxE2 <|> ctxE3)

ctxE1 (Expr (App f e)) = [ (e1, left <> p) | (e1, p) <- ctxE f ]
ctxE1 _ = []

ctxE2 (Expr (App (Expr (Lam x e1)) e2)) = 
  [ (e1', left <> down <> p) 
  | (e1', p) <- ctxE e1
  , e1' /= Expr (Var x) 
  ]
ctxE2 _ = []

ctxE3 (Expr (App (Expr (Lam x e1)) e2)) = 
  [ ( e2', right <> p) 
  | (v, _) <- ctxE e1
  , Expr (Var x) == v
  , (e2', p) <- ctxE e2
  ]
ctxE3 _ = []

demand x e = [ p | (e', p) <- ctxE e, e' == x ]

isValue (Expr (Lam _ _)) = True
isValue _ = False

isAnswer e@(Expr (App (Expr (Lam x e')) _)) = isAnswer e'
isAnswer e = isValue e

data AFRedexType = Deref | Lift | Assoc | Alpha | GC deriving (Enum,Eq,Show,Read)

reduce' :: Expr -> Maybe (AFRedexType, Expr)
-- Garbage collect comes first
reduce' (Expr (App (Expr (Lam x e1)) e2)) | notElem x (freeVars e1) = Just (GC, e1)
-- (\x.E[x]) v = (\x.E[v]) v
reduce' (Expr (App e@(Expr (Lam x e1)) e2)) 
  | d /= [] && isValue e2 = Just $
    case d of
      [p] -> 
        let fvs = freeVars e2
            e1' = fromZipper $ first (const e2) $ navigate p e1
        in if null (fvs `intersect` boundVars e)
          then (Deref, Expr (App (Expr (Lam x e1')) e2))
          else (Alpha, Expr (App (alpha fvs e) e2))
      x -> error $ "more than one " ++ show x ++ " is demanded in " ++ show e1 ++ ": " ++ show x
  where d = demand (Expr (Var x)) e1
-- (\x.a) e1 e2 = (\x.a e2) e1
reduce' e@(Expr (App (Expr (App (e'@(Expr (Lam x af))) e1)) e2)) 
  | isAnswer af = Just $
    let fvs2 = freeVars e2 
    in if elem x fvs2
      then (Alpha,) $ Expr $ App (Expr (App (alpha fvs2 e') e1)) e2
      else (Lift,) $ Expr $ App (Expr (Lam x (Expr (App af e2)))) e1
-- (\x . E[x])((\y.a) e) = (\y.(\x.E[x]) a) e
reduce' (Expr (App e@(Expr (Lam x e1)) (Expr (App e'@(Expr (Lam y e2)) e3)))) 
  | demand (Expr (Var x)) e1 /= [] && isValue e2 = Just $
    let fvs = freeVars e
    in if elem y fvs
      then (Alpha,) $ Expr $ App e (Expr (App (alpha fvs e') e3))
      else (Assoc,) $ Expr $ App (Expr (Lam y (Expr (App e e2)))) e3
reduce' e = Nothing

data AFRedex = AFRedex
  { afRedexPath :: Nav
  , afRedexType :: AFRedexType
  } deriving Eq

instance Redex AFRedex where
  redexes = concatMap toRedex . ctxE 
    where toRedex (e, ctx) = maybe [] (\(t, _) -> [AFRedex ctx t]) $ reduce' e
  reduce (AFRedex nv _) = fromZipper . first (snd . fromJust . reduce') . navigate nv
  redexDesc (AFRedex _ t) = show t
  hasPath (AFRedex p _) = (p==)

instance Show AFRedex where
  show (AFRedex nv r) = show nv ++ "(" ++ show r ++ ")"

instance Read AFRedex where
  readsPrec _ = mapP (uncurry AFRedex) (reads &&& paren reads)

