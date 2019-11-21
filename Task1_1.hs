module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Op = Add | Sub | Mul
    deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ op::Op, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant a) (IntConstant b) = IntConstant (a + b)
(|+|) a b = BinaryTerm Add a b
infixl 6 |+|

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant a) (IntConstant b) = IntConstant (a - b)
(|-|) a b = BinaryTerm Sub a b
infixl 6 |-|

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant a) (IntConstant b) = IntConstant (a * b)
(|*|) a b = BinaryTerm Mul a b
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant a) = IntConstant a
replaceVar varName replacement (Variable a)  
    | a == varName = replacement
    | otherwise    = Variable a
replaceVar varName replacement (BinaryTerm op a b)
    = BinaryTerm op (replaceVar varName replacement a) (replaceVar varName replacement b)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate t@(IntConstant _) = t
evaluate (Variable _)      = error "Not a constant"
evaluate (BinaryTerm Add a b) = evaluate a |+| evaluate b
evaluate (BinaryTerm Sub a b) = evaluate a |-| evaluate b
evaluate (BinaryTerm Mul a b) = evaluate a |*| evaluate b
