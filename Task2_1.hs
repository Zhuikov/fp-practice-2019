module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree |
                 Leaf (Integer, v) |
                 Node (Integer, v) (TreeMap v, TreeMap v)
    deriving (Show) 

testTree = Node (100, "100") ( 
                Node (50, "50") 
                    (Leaf (25, "25"), Node (75, "75")
                                            (Leaf(60, "60"), Leaf(80, "80")) ), 
                Node (150, "150") 
                    (EmptyTree, Leaf (200, "200")) )
{-
             100
         50     150
       25   75     200
          60  80
-}

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree k = False
contains (Leaf (key, value)) k = key == k
contains (Node (key, value) (left, right)) k 
    | key == k  = True
    | key < k   = contains right k
    | otherwise = contains left k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> Maybe v
lookup k EmptyTree = Nothing
lookup k (Leaf (key, value))
    | k == key  = Just value
    | otherwise = Nothing
lookup k (Node (key, value) (left, right))
    | k == key  = Just value
    | k < key   = lookup k left
    | otherwise = lookup k right

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert p EmptyTree = Leaf p
insert p'@(k, v) (Leaf p@(key, value))
    | k <= key  = Node p (Leaf p', EmptyTree)
    | otherwise = Node p (EmptyTree, Leaf p')
insert p'@(k, v) (Node p@(key, value) (left, right))
    | k == key  = Node p' (left, right)
    | k < key   = Node p (insert p' left, right)
    | otherwise = Node p (left, insert p' right)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove k EmptyTree = EmptyTree
remove k (Leaf (key, value))
    | k == key  = EmptyTree
    | otherwise = Leaf (key, value)
remove k n@(Node (key, value) ((Leaf (lKey, lValue), EmptyTree)))
    | k == lKey = Leaf (key, value)
    | k == key  = Leaf (lKey, lValue)
    | otherwise = n
remove k n@(Node (key, value) (EmptyTree, (Leaf (rKey, rValue))))
    | k == rKey = Leaf (key, value)
    | k == key  = Leaf (rKey, rValue)
    | otherwise = n
remove k (Node (key, value) (left, right))
    | k < key   = Node (key, value) (remove k left, right) 
    | k > key   = Node (key, value) (left, remove k right)
    | otherwise = Node (minKey, minValue) (left, remove minKey right)
        where (minKey, minValue) = findRightMin right
              findRightMin (Leaf p)                = p
              findRightMin (Node p (EmptyTree, _)) = p
              findRightMin (Node _ (left, _))      = findRightMin left


-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> Maybe (Integer, v)
nearestLE _ EmptyTree = Nothing
nearestLE k (Leaf p@(key, value)) 
    | k >= key  = Just p
    | otherwise = Nothing
nearestLE k (Node p@(key, value) (left, right)) 
    | k == key  = Just p
    | k < key   = nearestLE k left
    | otherwise = case nearestLE k right of
            r@(Just (_, _)) -> r
            Nothing -> Just p

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList = foldr insert EmptyTree


-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Leaf p)  = [p]
listFromTree (Node p (left, right)) = listFromTree left ++ [p] ++ listFromTree right

--listFromTree testTree = [(25,"25"),(50,"50"),(60,"60"),(75,"75"),(80,"80"),(100,"100"),(150,"150"),(200,"200")]

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = listFromTree t !! (fromIntegral i)
