module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where

-- Применение функции f к результату фукнции fun, лежащей в контексте монады.
-- Создание контекста с новым поведением.
-- Functor f => (a -> b) -> f a -> f b

    fmap f (FunMonad fun) = FunMonad (f . fun)

{-
    a = FunMonad length
    f = (+10)
    fun (fmap f a) "abc" === 13
-}

-- Закон fmap id x === x выполнен

instance Applicative FunMonad where
    
-- Обернуть в FunMonad переданную функцию.

    pure f = FunMonad (\x -> f)

-- Applicative f => f (a -> b) -> f a -> f b
-- Заменяя, a на (String -> a) получим:
-- Applicative f => f (String -> a -> b) -> f (String -> a) -> f b
-- Таким образом, fun1 принимает два аргумента: String и a, возвращая b.

    (<*>) (FunMonad fun1) (FunMonad fun2) = FunMonad (\x -> fun1 x $ fun2 x )

{- 
    Пример.

    a :: Integral a => t -> a -> Bool
    a str len = even $ len

    b :: Foldable t => t a -> Int
    b = length

    a' = FunMonad a
    b' = FunMonad b
    c  = a' <*> b'

    fun c "123" === False
-}


instance Monad FunMonad where
    
    return = pure

-- Monad m => m a -> (a -> m b) -> m b
-- FunMonad (String -> a) -> (a -> FunMonad (String -> b) -> FunMonad (String -> b)
-- fun1 x - применили функцию из монады к аргументу - получили тип a
-- f $ fun1 x - получили FunMonad (String -> b)
-- fun (f $ fun1 x) - "достали" из полученной монады функцию
-- fun (f $ fun1 x) x - применили эту функцию к аргументу.
-- Получили неоходимую монаду

    (>>=) (FunMonad fun1) f = FunMonad (\x -> fun (f $ fun1 x) x )

