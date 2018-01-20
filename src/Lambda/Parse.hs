{-# LANGUAGE FlexibleContexts #-}
module Lambda.Parse (parseTerm, propParse) where

import Lambda.Term
import Lambda.Show
import Lambda.Read (isAlphaNum, isAlpha, isDigit, isSpace)
import Control.Applicative ((<$>),(<*>))
import Text.ParserCombinators.Parsec hiding (getInput, State)

--We also provide a Parsec parser to read in Term for nicer error messages

m &&& n = do
  x <- m
  y <- n
  return (x, y)

variable = f <$> (alpha &&& many digit)
  where f (c, d) = V (c : d)
        alpha = satisfy isAlpha
        digit = satisfy isDigit

paren = between (sym '(') (sym ')')
term = lam <|> (foldl1 ((.) Expr . App) <$> many1 atom)
atom = lam <|> var <|> paren term

var = trim $ Expr . Var <$> variable

lam = f <$> (lbd &&& variable &&& sym '.' &&& term)
  where f (((_, v), _), e) = Expr $ Lam v e

lbd = sym '\\' <|> sym 'λ'

sym = trim . char

trim p = f <$> (spaces &&& p &&& spaces)
  where f ((_, x), _) = x

parseTerm :: String -> Either ParseError Expr
parseTerm e = parse (term >>= \e -> eof >> return e) "" e

--The following property should pass for Quickcheck, which can be
--verified by "quickCheck propTerm".

propParse :: Expr -> Bool
propParse t = either (const False) (t==) (parseTerm (show t))


