{-# LANGUAGE NegativeLiterals #-}
module Derive where

data Expr =
    Const Integer
  | X
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  | Expr :^ Integer
  | Expr :~> Expr -- f(g(x)) = g :~> f
    deriving (Show, Eq)

--f(x) = x^2
--X :~> x :* x
--f(x*x) :~> X :* X
--(X :* X) :* (X :* X)


infixl 6 :+
infixl 6 :-
infixl 7 :*
infixl 7 :/
infixr 8 :^
infixr 5 :~>

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



simplify :: Expr -> Expr
-- Sum Rules
simplify (a :+ Const 0) = a
simplify (Const 0 :+ a) = a
simplify (a :+ b) | simplify a == simplify b = Const 2 :* a
simplify (Const n :* a :+ b) | simplify a == simplify b = Const (n+1) :* a

-- Product rules
simplify (_ :* Const 0) = Const 0
simplify (Const 0 :* _) = Const 0
simplify (a :* Const 1) = a
simplify (Const 1 :* a) = a

-- Difference rules
simplify (a :- Const 0) = a
simplify (Const 0 :- a)  = (Const -1) :* a

-- Quotient Rules
simplify (a :/ Const 1) = a

-- Power Rules
simplify (_ :^ 0) = Const 1
simplify (a :^ 1) = simplify a


-- Constant arithmatic
simplify (Const a :+ Const b) = Const (a + b)
simplify (Const a :* Const b) = Const (a * b)
simplify (Const a :- Const b) = Const (a - b)
--simplify (Const a :/ Const b) = Const (a / b) -- Doesn't work with integers
simplify (Const a :^ n) = Const (a ^ n)

-- Function Rules
simplify (_ :~> Const c) = Const c
simplify (X :~> f) = simplify f

-- Basic rules
simplify (Const c) = Const c
simplify X = X
simplify (a :+ b) = simplify a :+ simplify b
simplify (a :- b) = simplify a :- simplify b
simplify (a :* b) = simplify a :* simplify b
simplify (a :/ b) = simplify a :/ simplify b
simplify (a :^ n) = simplify a :^ n
simplify (f :~> g) = simplify f :~> simplify g


sdx :: Expr -> Expr
sdx = tillCycle simplify . dx

tillCycle :: Eq a => (a -> a) -> a -> a
tillCycle trans start = go start
    where go current
            | next == current = next
            | otherwise       = go next
            where next = trans current
