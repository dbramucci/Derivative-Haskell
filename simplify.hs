{-# LANGUAGE NegativeLiterals #-}
module Simplify (simplify) where

import Expression (Expr(..))

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
simplify (a :* b) | simplify a == simplify b = a :^ 2
simplify (a :^ n :* b) | simplify a == simplify b = a :^ (n + 1)
simplify (a :* b :^ n) | simplify a == simplify b = a :^ (n + 1)

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
simplify (Const a :+ (Const b :+ c)) = Const (a + b) :+ simplify c
simplify (Const a :* Const b) = Const (a * b)
simplify (Const a :* (Const b :* c)) = Const (a * b) :+ simplify c
simplify (Const a :- Const b) = Const (a - b)
--simplify (Const a :/ Const b) = Const (a / b) -- Doesn't work with integers
simplify (Const a :^ n) = Const (a ^ n)

-- Function Rules
simplify (_ :~> Const c) = Const c
simplify (X :~> f) = simplify f

-- Commutivity and Associativity rules
simplify (a :+ Const c) = Const c :+ simplify a
simplify (a :* Const c) = Const c :* simplify a
simplify (a :+ (Const c :+ b)) = Const c :+ (simplify a :+ simplify b)
simplify (a :* (Const c :* b)) = Const c :* (simplify a :* simplify b)
simplify ((a :+ b) :+ c) = simplify a :+ (simplify b :+ simplify c)
simplify ((a :* b) :* c) = simplify a :* (simplify b :* simplify c)


-- Basic rules
simplify (Const c) = Const c
simplify X = X
simplify (a :+ b) = simplify a :+ simplify b
simplify (a :- b) = simplify a :- simplify b
simplify (a :* b) = simplify a :* simplify b
simplify (a :/ b) = simplify a :/ simplify b
simplify (a :^ n) = simplify a :^ n
simplify (g :~> f) = substitute (simplify g) (simplify f)
simplify (Sin a) = Sin (simplify a)
simplify (Cos a) = Cos (simplify a)
simplify (Tan a) = Tan (simplify a)

substitute :: Expr -> Expr -> Expr
substitute arg body =
    case body of
        Const c -> Const c
        X       -> arg
        a :+ b  -> substitute arg a :+ substitute arg b
        a :* b  -> substitute arg a :* substitute arg b
        a :- b  -> substitute arg a :- substitute arg b
        a :/ b  -> substitute arg a :/ substitute arg b
        a :^ n  -> substitute arg a :^ n
        -- Don't substitute into f because X in f refers to g
        g :~> f  -> substitute arg g :~> f
        Sin a -> Sin (substitute arg a)
        Cos a -> Cos (substitute arg a)
        Tan a -> Tan (substitute arg a)
