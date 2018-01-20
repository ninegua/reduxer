module Lambda.Test where

import Lambda.Term
import Control.Applicative
import Test.QuickCheck 

instance Arbitrary Var where
  arbitrary = fmap (V . (:[])) $ choose ('a', 'e')
{-
  arbitrary = do
    f <- choose ('a', 'z')
    c <- choose (False, True)
    d <- choose (0::Int, 100)
    return $ V $ if c then [f] else f : show d
-}

instance Arbitrary a => Arbitrary (Term a) where
  arbitrary = sized term
    where
      term 0 = Var <$> arbitrary
      term n = oneof [ Var <$> arbitrary, 
                       Lam <$> arbitrary <*> resize (n-1) arbitrary, 
                       App <$> resize m arbitrary <*> resize m arbitrary ]
        where m = n `div` 2

instance Arbitrary Expr where
   arbitrary = Expr <$> arbitrary
