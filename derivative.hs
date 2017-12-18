{-# LANGUAGE NegativeLiterals #-}
module Derive where

import Utility (fixpoint)
import Expression (Expr(..))
import Simplify (simplify)


dx :: Expr -> Expr
dx (Const _) = Const 0
dx X = Const 1
dx (a :+ b) = dx a :+ dx b -- d/dx (a + b) = (d/dx a) + (d/dx b)
dx (a :- b) = dx a :- dx b -- d/dx (a + b) = (d/dx a) - (d/dx b)
dx (a :* b) = dx a :* b :+ a :* dx b -- d/dx (a * b) = (d/dx a)b * a(d/dx b)
dx (a :/ b) =  (dx a :* b :- a :* dx b) :/ (b :* b) -- d/dx (a / b) = ((d/dx a)b - a(d/dx b))(b^2)
dx (X :^ n) = Const n :* (X :^ (n - 1))
dx (a :^ n) = dx (a :~> X :^ n)
dx (g :~> f) = (g :~> dx f) :* dx g
dx (Sin X) = Cos X
dx (Cos X) = Const -1 :* Sin X
dx (Sin a) = dx (a :~> Sin X)
dx (Cos a) = dx (a :~> Cos X)
dx (Tan a) = dx (Sin a :/ Cos a)


sdx :: Expr -> Expr
sdx = fixpoint simplify . dx
