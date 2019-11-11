module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = logic x 10 0 where
    logic _ (-1) sum = sum
    logic x n sum = logic x (n - 1) ( sum + (-1)**n * x**(2 * n) / product [1..2 * n] ) 

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y = logic (abs x) (abs y) where
    logic x 0 = x
    logic 0 y = y
    logic x y | x > y  = logic y (x `mod` y)
              | x < y  = logic x (y `mod` x)
              | x == y = x

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to | from > to = False
                               | otherwise = not $ null squaresBetweenFromTo where
    squaresBetweenFromTo = filter (>= from) $ takeWhile (< to) $ map (^2) [0..]

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = todo

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = logic $ abs x where
    logic 1 = False
    logic x | even x    = x == 2
            | otherwise = null [ num | num <- [3,5..x - 1], x `mod` num == 0 ]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
