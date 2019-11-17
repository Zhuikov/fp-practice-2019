module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ini []     = ini
foldl f ini (x:xs) = foldl f (f ini x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f ini []     = ini
foldr f ini (x:xs) = f x $ foldr f ini xs

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
    helper (Just (x, ini')) = x : unfoldr f ini'
    helper Nothing          = []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum = foldl (+) 0

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse = foldl f [] where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x s -> f x : s) [] 

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldl (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
    where f (Just x) s = x : s
          f Nothing  s = s

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal list = foldr f [] list
    where f x s = x !! (length list - length s - 1) : s

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = foldr (\x s -> if not $ p x then x : s else s) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem a = foldr (\x s -> if x == a then True else s) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = 
    unfoldr (\x -> if x < to then Just (x, x + step) else Nothing) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append list1 list2 = foldr (:) list2 list1


-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr fun lst 
    where fun []   = Nothing
          fun list = Just (take (fromIntegral n) list, drop (fromIntegral n) list)
