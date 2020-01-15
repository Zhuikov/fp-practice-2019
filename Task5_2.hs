module Task5_2 where

import Todo(todo)
import Prelude hiding (concat)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

instance (Show a) => Show (Zipper a)  where
    show z = show r
        where (Zipper l r) = toLeftEnd z

instance (Eq a) => Eq (Zipper a) where

    (==) z1 z2 = r1 == r2
        where (Zipper _ r1) = toLeftEnd z1
              (Zipper _ r2) = toLeftEnd z2

-- Выводит внутреннее состояние зиппера
showParts :: (Show a) => Zipper a -> String
showParts (Zipper l r) = show l ++ " " ++ show r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

-- Функция перемещения указателя зиппера в левый конец
-- Правый список в зиппере в таком случае соотвествует первоначальному списку (см. fromList)
toLeftEnd :: Zipper a -> Zipper a
toLeftEnd z@(Zipper l r) = logic z $ length l
    where logic z 0 = z
          logic z n = logic (goLeft z) $ n - 1

concat :: Zipper a -> Zipper a -> Zipper a
concat left@(Zipper l1 r1) right = Zipper l1 (r1 ++ r2)
    where (Zipper _ r2) = toLeftEnd right

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index what@(Zipper left right) to@(Zipper l r)
    | index > length l  = insertManyAt index what $ goRight to
    | index < length l  = insertManyAt index what $ goLeft to
    | index == length l = Zipper (left ++ l) (right ++ r)

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to input@(Zipper l r) 
    | from > length l  = subZipper from to $ goRight input
    | from < length l  = subZipper from to $ goLeft input
    | from == length l = fromList $ drop 0 . take (to - from) $ r

{-
    a = fromList [1,2,3,4,5]
    b = fromList [10,11,12,13]

    c = concat a b
    c === [1,2,3,4,5,10,11,12,13]

    ins = fromList [(-1), (-2), (-3), (-4), (-5)]
    ins1 = goRight ins
    ins2 = goRight ins1

    ins2 === [(-1), (-2), (-3), (-4), (-5)]
    showParts ins2 === "[-2,-1] [-3,-4,-5]"

    res = insertManyAt 4 ins2 c
    res === [1,2,3,4,-1,-2,-3,-4,-5,5,10,11,12,13]
    showParts res == "[-2,-1,4,3,2,1] [-3,-4,-5,5,10,11,12,13]"
-}
