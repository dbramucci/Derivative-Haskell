module Expression (Expr(..)) where

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
