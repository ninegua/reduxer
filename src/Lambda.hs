module Lambda (
    Var(..), Term(..), Expr(..), 
    Dir(..), Nav(..), AExpr(..), NavAExpr, Zipper, 
    fold, build, traverse, 
    freeVars, subst, 
    Redex, redexes, reduce, redexDesc, hasPath, 
    BasicRedex, LeftOuterRedex, LeftInnerRedex, CBNRedex, CBVRedex, CbnRedex, AFRedex, CSFRedex,
    alpha, -- beta, alphaOrBeta, 
    navigate, fromZipper, toNavAExpr, fromNavAExpr
  ) where
import Prelude hiding (traverse)
import Lambda.Term
import Lambda.Read
import Lambda.Show
import Lambda.Nav
import Lambda.Strategy 
import Lambda.AriolaFelleisen
import Lambda.CallByNeed
import Lambda.CSF
import Lambda.Test
