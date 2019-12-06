module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons list a) = a : rlistToList list

listToRList :: [a] -> ReverseList a
listToRList []     = RNil
listToRList (x:xs) = RCons (listToRList xs) x

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
    show rlist = "[" ++ helper rlist ++ "]"
        where
            helper RNil           = ""
            helper (RCons RNil x) = show x
            helper (RCons list x) = show x ++ "," ++ helper list

instance (Eq a) => Eq (ReverseList a) where
    
-- Два списка равны, если их длины равны, а также равны соответствующие элементы
    (==) RNil RNil = True
    (==) RNil _    = False
    (==) _ RNil    = False
    (==) (RCons list1 x1) (RCons list2 x2) 
            | x1 /= x2 = False
            | x1 == x2 = list1 == list2

instance (Ord a) => Ord (ReverseList a) where

    compare list1 list2 | list1 == list2 = EQ
    compare RNil _ = LT
    compare _ RNil = GT
    compare (RCons list1 x1) (RCons list2 x2) 
        | x1 == x2  = compare list1 list2
        | x1 > x2   = GT
        | x1 < x2   = LT

instance Monoid (ReverseList a) where
    
    mempty = RNil
    mappend list1 RNil = list1
    mappend RNil list2 = list2
    mappend list1 list2 = mappend (tail list1) (RCons list2 $ first list1)
        where
            first (RCons RNil x) = x
            first (RCons l x)    = first l
            tail (RCons RNil x)  = RNil
            tail (RCons l x)     = (RCons (tail l) x)

instance Functor ReverseList where 

    fmap f RNil = RNil
    fmap f (RCons list x) = RCons (fmap f list) $ f x

