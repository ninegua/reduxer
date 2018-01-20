module Lambda.Read where

import Lambda.Term
import Lambda.Show

-- Basic parser combinators.
nil :: ReadS [a]
nil s = [([], s)]

char :: (Char -> Bool) -> ReadS Char
char f (c:s) | f c = [(c, s)]
char f _           = []

-- A functor indeed!
mapP :: (a -> b) -> ReadS a -> ReadS b
mapP f g = map (\ (c, s) -> (f c, s)) . g

-- Sequencing
(&&&) :: ReadS a -> ReadS b -> ReadS (a, b)
f &&& g = \s -> [ ((x, y), s2) 
                | (x, s1) <- f s, 
                  (y, s2) <- g s1 ]

f -&& g = mapP snd (f &&& g)
f &&- g = mapP fst (f &&& g)

-- Alternative 
(|||) :: ReadS a -> ReadS b -> ReadS (Either a b)
f ||| g = \s -> map left (f s) ++ map right (g s)
  where left  (x, s) = (Left  x, s)
        right (y, s) = (Right y, s)

(<|>) :: ReadS a -> ReadS a -> ReadS a
f <|> g = mapP select (f ||| g)
        where select (Left  x) = x
              select (Right y) = y

-- Alternative with a strong left bias.
(|<|) :: ReadS a -> ReadS b -> ReadS (Either a b)
f |<| g = \s -> case f s of
                  [] -> map right (g s)
                  xs -> map left xs
  where left  (x, s) = (Left  x, s)
        right (y, s) = (Right y, s)

(<|) :: ReadS a -> ReadS a -> ReadS a
f <| g = mapP select (f |<| g)
  where select (Left  x) = x
        select (Right y) = y

many, many1 :: ReadS a -> ReadS [a]
many r = (mapP (uncurry (:)) (r &&& many r)) <|> nil
many1 r = mapP (uncurry (:)) (r &&& many r)

manySep, manySep1 :: ReadS b -> ReadS a -> ReadS [a]
manySep s r = (mapP (uncurry (:)) (s -&& r &&& many r)) <|> nil
manySep1 s r = mapP (uncurry (:)) (r &&& manySep s r)

-- Read instance for Var and Term.
instance Read Var where
  readsPrec _ = variable 

instance Read Expr where
  readsPrec _ = term 

paren p = sym '(' -&& p &&- sym ')'

-- Parser for variables that start with lowercase letter,
-- and optionally followed by a number.
variable = mapP f (alpha &&& (digits <|> nil))
  where f (c, d) = V (c : d)
        alpha = char isAlpha
        digits = many1 (char isDigit)

-- Parser for lambda terms that is not ambiguous, and not left-recursive.
-- term :: ReadS Expr
term = term' Expr term

term' :: (Term a -> a) -> ReadS a -> ReadS a
term' f t = mapP f (lam t) <| mapP (foldl1 ((f.).App)) (many1 (atom f t))

atom :: (Term a -> a) -> ReadS a -> ReadS a
atom f t = mapP f (lam t <|> var) <|> paren t

var :: ReadS (Term a) 
var = trim $ mapP Var variable

lam :: ReadS a -> ReadS (Term a)
lam t = mapP (uncurry Lam) $ (lbd -&& variable) &&& (sym '.' -&& t)

lbd = (sym '\\' <|> sym 'λ')

sym = trim . char . (==)

word = foldr1 (-&&) . map sym

-- Aggressively match as many space as possible
space :: ReadS String
space = mapP (uncurry (:)) (char isSpace &&& (space <| nil))

-- Trim all spaces around parser p
trim :: ReadS a -> ReadS a
trim p = ((space |<| nil) -&& p) &&- (space |<| nil)

-- Only match ASCII
isAlphaNum x = x >= 'a' && x <= 'z' || x >= '0' && x <= '9'
isAlpha c = c >= 'a' && c <= 'z'
isDigit c = c >= '0' && c <= '9'
isSpace c = c == ' ' || c == '\t'
isUpper c = c >= 'A' && c <= 'Z'

propRead :: Expr -> Bool
propRead t = read (show t) == t
