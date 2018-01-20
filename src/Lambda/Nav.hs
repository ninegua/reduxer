{-# Language FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Lambda.Nav where
import Prelude hiding (traverse)
import Lambda.Term
import Lambda.Show
import Data.Monoid
import Control.Monad.Reader
import Control.Applicative

-- how to navigate a lambda AST.
data Dir = U | D | L | R deriving (Eq, Show, Enum)
newtype Nav = Nav [Dir] deriving (Eq, Monoid)
type Zipper = (Expr, [Expr -> Expr])

instance Show Nav where
  show (Nav ns) = concat $ map show $ ns

instance Read Nav where
  readsPrec _ = toNav . aux []
    where
      toNav (ns, s) = [(Nav ns, s)]
      aux ns ('U':s) = aux (U:ns) s
      aux ns ('D':s) = aux (D:ns) s
      aux ns ('L':s) = aux (L:ns) s
      aux ns ('R':s) = aux (R:ns) s
      aux ns s       = (reverse ns, s)

up, down, left, right :: Nav
up = Nav [U]
down = Nav [D]
left = Nav [L]
right = Nav [R]

navigate :: Nav -> Expr -> Zipper
navigate (Nav ns) e = foldl (flip aux) (e, []) ns
  where 
    aux U (e,            h:hs) = (h e, hs)
    aux D (Expr (Lam v e), hs) = (e, Expr . Lam v : hs)
    aux L (Expr (App f e), hs) = (f, Expr . flip App e : hs)
    aux R (Expr (App f e), hs) = (e, Expr . App f : hs) 
    aux _ _ = error $ "invalid navigation path " ++ show ns ++ " on " ++ show e

fromZipper :: Zipper -> Expr
fromZipper (e, hs) = foldl (flip ($)) e hs

allLamNavs :: Expr -> [Nav]
allLamNavs = map Nav . fold aux
  where
    aux (Lam _ e) = [] : map (D:) e
    aux (App f e) = map (L:) f ++ map (R:) e
    aux _ = []

-- To allow annotations, we extend the original Term with additional information
data AExpr a = AExpr a (Term (AExpr a)) deriving Eq

-- Expr annotated with Nav 
type NavAExpr = AExpr Nav

unAExpr (AExpr _ t) = t

toNavAExpr :: Expr -> NavAExpr
toNavAExpr e = runReader (fold (aux . dist) e) []
  where
    dist (Var v) = return (Var v)
    dist (Lam v e) = Lam v <$> local (D:) e
    dist (App f e) = App <$> local (L:) f <*> local (R:) e
    aux e = AExpr <$> fmap (Nav . reverse) ask <*> e

fromNavAExpr :: NavAExpr -> Expr
fromNavAExpr = traverse (\h -> Expr . h . unAExpr)

-- test if toNavAExpr gives correct navigation path to sub trees
propNavA :: Expr -> Bool
propNavA e = traverse verify (toNavAExpr e)
  where
    verify h a@(AExpr ns t) = fromNavAExpr a == fst (navigate ns e) && check (h t)
    check (Lam _ True) = True
    check (App True True) = True
    check (Var _) = True
    check _ = False


