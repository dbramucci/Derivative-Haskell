{-# LANGUAGE NegativeLiterals #-}
module Derive where

import Utility (fixpoint)


data Expr =
    Const Integer
  | X
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  | Expr :^ Integer
  | Expr :~> Expr -- f(g(x)) = g :~> f
  | Sin Expr
  | Cos Expr
  | Tan Expr
    deriving Eq

-- Define Function associativity and precedence.
infixl 6 :+
infixl 6 :-
infixl 7 :*
infixl 7 :/
infixr 8 :^
infixr 5 :~>


isAtomic :: Expr -> Bool
isAtomic a = case a of
    Const _ -> False
    X -> True
    _ :+ _ -> False
    _ :- _ -> False
    _ :* _ -> False
    _ :/ _ -> False
    _ :^ _ -> False
    _ :~> _ -> False
    Sin _ -> True
    Cos _ -> True
    Tan _ -> True

needSurroundingParen :: Expr -> Int -> Bool
needSurroundingParen x m = case x of
    Const _ -> False
    X       -> False
    a :+ b -> m > 6 || needSurroundingParen a m || needSurroundingParen b m
    a :- b -> m > 6 || needSurroundingParen a m || needSurroundingParen b m
    a :* b -> m > 7 || needSurroundingParen a m || needSurroundingParen b m
    a :/ b -> m > 7 || needSurroundingParen a m || needSurroundingParen b m
    a :^ _ -> m > 8 || needSurroundingParen a m
    a :~> b -> m > 5 || needSurroundingParen a m || needSurroundingParen b m
    Sin _  -> False
    Cos _  -> False
    Tan _  -> False

addParens :: String -> String
addParens x = "(" ++ x ++ ")"

instance Show Expr where
    show (Const n) = show n
    show X = "x"
    show e@(a :+ b) = (if needSurroundingParen e 6 then addParens else id) (show a ++ " + " ++ show b)
    show e@(a :- b) = (if needSurroundingParen e 6 then addParens else id) (show a ++ "-" ++ addParens(show b))
    show e@(a :* b) = (if needSurroundingParen e 7 then addParens else id) (show a ++ " * " ++ show b)
    show e@(a :/ b) = (if needSurroundingParen e 7 then addParens else id) (show a ++ " / " ++ addParens (show b))
    show e@(a :^ n) = (if needSurroundingParen e 8 then addParens else id) (show a ++ " ^ " ++ show n)
    show e@(g :~> f) = (if needSurroundingParen e 5 then addParens else id) (show g ++ " -> " ++ show f)
    show (Sin a) = "sin" ++ addParens (show a)
    show (Cos a) = "cos" ++ addParens (show a)
    show (Tan a) = "tan" ++ addParens (show a)




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
