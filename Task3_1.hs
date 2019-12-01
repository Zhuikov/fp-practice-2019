module Task3_1 where

import Data.Ratio

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

    deriving(Show)

testPeano1 = Succ $ Succ $ Succ $ Succ Zero
testPeano2 = Pred $ Pred $ Pred $ Zero

instance Num WeirdPeanoNumber where
    
    negate Zero     = Zero
    negate (Succ x) = Pred $ negate x
    negate (Pred x) = Succ $ negate x

    abs Zero     = Zero
    abs (Succ x) = Succ x
    abs (Pred x) = Succ (negate x)

    (+) Zero x              = x
    (+) s@(Succ x) (Succ y) = Succ (s + y)
    (+) p@(Pred x) (Pred y) = Pred (p + y)
    (+) (Succ x) (Pred y)   = x + y
    (+) x y                 = y + x

    (*) Zero x                  = Zero
    (*) s@(Succ x) (Succ y)     = s + (s * y)
    (*) p1@(Pred x) p2@(Pred y) = (negate p1) * (negate p2)
    (*) s@(Succ x) p@(Pred y)   = negate $ s * negate p
    (*) x y                     = y * x

    signum Zero     = Zero
    signum (Succ x) = Succ Zero
    signum (Pred x) = Pred Zero

    fromInteger 0 = Zero
    fromInteger x | x < 0 = Pred (fromInteger $ x + 1)
                  | x > 0 = Succ (fromInteger $ x - 1)


instance Eq WeirdPeanoNumber where
    
    (==) x y = case (x - y) of
        Zero -> True
        otherwise -> False

instance Ord WeirdPeanoNumber where

    compare x y = case signum (x - y) of 
        Zero -> EQ
        (Pred Zero) -> LT
        (Succ Zero) -> GT

instance Integral WeirdPeanoNumber where

    toInteger Zero = 0
    toInteger (Succ x) = toInteger x + 1
    toInteger (Pred x) = toInteger x - 1

    quotRem _ Zero = error "Divide by zero"
    quotRem Zero _ = (Zero, Zero)
    quotRem x y | x < 0          = (negate d, negate m) where (d, m) = quotRem (negate x) y
    quotRem x y | y < 0          = (negate d, m) where (d, m) = quotRem x (negate y)
    quotRem x y | x > 0 && y > 0 = helper x y (Zero, Zero)
        where helper x y (d, m) | x < y     = (d, m)
                                | otherwise = helper (x - y) y (d + 1, x - y)

instance Enum WeirdPeanoNumber where

    toEnum x   = fromInteger $ toInteger x
    fromEnum x = fromEnum $ toInteger x

instance Real WeirdPeanoNumber where

    toRational x = toInteger x % 1

