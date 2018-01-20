{-# LANGUAGE TupleSections #-}
module Lambda.CallByNeed where

import Lambda.Term
import Lambda.Nav
import Lambda.Show
import Lambda.Read ((&&&), (<|>), mapP, sym, paren)
import Lambda.Strategy hiding (BasicRedexType(..))
import Lambda.Test
import Data.Monoid
import Data.Maybe 
import Control.Arrow (first)
import Data.List (intersect, nub)
import Lambda.AriolaFelleisen (AFRedexType(..)) 
import qualified Lambda.AriolaFelleisen as AF

-- A slight modification of AriolaFelleisen by changing Deref to Beta rule:
--     (\x.E[x]) v = E[v]{x := v}

data CbnRedexType = AFType AFRedexType | Beta deriving Eq

instance Show CbnRedexType where
  show Beta = "Beta"
  show (AFType t) = show t

instance Read CbnRedexType where
  readsPrec _ = (mapP (const Beta) $ sym 'B' &&& sym 'e' &&& sym 't' &&& sym 'a') <|> 
                (mapP AFType reads)

data CbnRedex = CbnRedex
  { cbnRedexPath :: Nav
  , cbnRedexType :: CbnRedexType
  } deriving Eq

reduce' :: Expr -> Maybe (CbnRedexType, Expr)
reduce' e@(Expr (App (Expr (Lam x e1)) e2)) | d /= [] && AF.isValue e2 = 
  Just $ either (AFType Alpha,) (Beta,) $ alphaOrBeta e
  where d = AF.demand (Expr (Var x)) e1
reduce' e = fmap (first AFType) $ AF.reduce' e

instance Redex CbnRedex where
  redexes = concatMap toRedex . AF.ctxE 
    where toRedex (e, ctx) = maybe [] (\(t, _) -> [CbnRedex ctx t]) $ reduce' e
  reduce (CbnRedex nv _) = fromZipper . first (snd . fromJust . reduce') . navigate nv
  redexDesc (CbnRedex _ t) = show t
  hasPath (CbnRedex p _) = (p==)

instance Show CbnRedex where
  show (CbnRedex nv r) = show nv ++ "(" ++ show r ++ ")"

instance Read CbnRedex where
  readsPrec _ = mapP (uncurry CbnRedex) (reads &&& paren reads)
