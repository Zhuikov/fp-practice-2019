module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов

lenDList list = helper list 0
    where
        helper DNil len = len
        helper (DCons l x r) len = helper r $ len + 1

index :: DList a -> Int -> a
index DNil _ = error "Out of bounds"
index (DCons left x right) i
    | i == 0 = x
    | otherwise = index right $ i - 1

insertAt :: DList a -> Int -> a -> DList a
insertAt list index value 
    | index >= lenDList list || index < 0 = error "Out of bounds"
    | otherwise = helper DNil list index value
    where 
        helper left DNil index value = 
            if index == 0 then DCons left value DNil else error "Out of bounds"
        helper left (DCons l x r) index value 
            | index == 0 = let rec = DCons l value (DCons rec x r) in rec
            | otherwise = let rec = DCons l x (helper rec r (index - 1) value) in rec

removeAt :: DList a -> Int -> DList a
removeAt list index 
    | index >= lenDList list || index < 0 = error "Out of bounds"
    | otherwise = helper DNil list index
    where
        helper left (DCons left' x' right') i
            | i == 0 = case right' of
                DNil -> DNil
                DCons l x r -> DCons left x r
            | otherwise = let rec = DCons left' x' (helper rec right' $ i - 1) 
                          in rec

