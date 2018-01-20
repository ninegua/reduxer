module Lambda.Show where

import Lambda.Term

-- Render a Term to string, and only use paranthesis when necessary.
instance Show Expr where
  show = snd . fold showTerm

showTerm (Var (V v)) = (either id id, v)
showTerm (Lam (V v) (_,e)) = (either pr pr, "λ" ++ v ++ "." ++ e)
showTerm (App (b,f) (d,e)) = (either id pr, b (Left f) ++ " " ++ d (Right e))

pr s = "(" ++ s ++ ")" 
