{-# LANGUAGE TupleSections, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lambda.Term where
import Data.List (delete, union, intersect)
import Control.Applicative
import Control.Monad ((>=>))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

-- The main data type Term is inductively defined.
data Term a = Var Var | Lam Var a | App a a deriving (Eq, Functor, Foldable, Traversable)
newtype Var = V String deriving (Eq, Show)

newtype Expr = Expr { unExpr :: Term Expr } deriving Eq

-- bottom-up fold 
fold :: (Term a -> a) -> Expr -> a
fold f = f . fmap (fold f) . unExpr

-- top-down build
build :: (a -> Term a) -> a -> Expr
build f = Expr . fmap (build f) . f 

-- generalized traversal
-- ffix :: ((Term Expr -> Term a) -> Term Expr -> a) -> Expr -> a
ffix :: Functor f => ((f a -> f b) -> a -> b) -> a -> b
ffix f = f (fmap (ffix f))

-- the following are equivalent to fold and build
fold'  f = ffix (\h -> f . h . unExpr)
build' f = ffix (\h -> Expr . h . f)
{-
foldWith, foldWith' :: (Expr -> Term a -> a) -> Expr -> a
foldWith f e = f e $ fmap (foldWith f) $ unExpr e
foldWith' f = ffix (\h e -> f e (h (unExpr e)))
-}

-- Monadic fold and build are not much more than a fold or build, with a 
-- monadic distribute function:
--
--  foldm  dist f = fold  (f . dist)
--  buildm dist f = build (dist . f)
--
-- The usual definition of dist is given below, which is equivalent to the
-- sequenceA function in Traversable class. However, there are occasions
-- where a custom dist function is desirable.
dist :: Applicative m => Term (m a) -> m (Term a)
dist (Var v) = pure (Var v)
dist (Lam v e) = (Lam v) <$> e
dist (App f e) = App <$> f <*> e

freeVars :: Expr -> [Var]
freeVars = fold freeVars'

freeVars' :: Term [Var] -> [Var]
freeVars' (Var v)   = [v]
freeVars' (Lam v s) = delete v s
freeVars' (App s t) = union s t

boundVars :: Expr -> [Var]
boundVars = fold boundVars'

boundVars' :: Term [Var] -> [Var]
boundVars' (Var v)   = []
boundVars' (Lam v s) = v : s
boundVars' (App s t) = union s t

subst :: Var -> Expr -> Expr -> Expr
subst u w = ffix aux
  where
    aux h   (Expr (Var v))   | v == u = w
    aux h e@(Expr (Lam v _)) | v == u = e
    aux h   (Expr t)                  = Expr (h t)

{- A more conventional definition of subst, used for verification -}
subst' :: Var -> Expr -> Expr -> Expr
subst' u w = Expr . aux . unExpr
  where
    aux (Var v)   = if v == u then unExpr w else Var v
    aux (Lam v e) = if v == u then Lam v e else Lam v (subst' u w e)
    aux (App f e) = App (subst' u w f) (subst' u w e)

propSubst :: Int -> Expr -> Expr -> Bool
propSubst i e t = 
  let fvs = freeVars e
      bvs = boundVars e
      vs = fvs `union` bvs
      v = if null fvs then (V "a") else vs !! (abs i `mod` (length vs))
  in subst v t e == subst' v t e


