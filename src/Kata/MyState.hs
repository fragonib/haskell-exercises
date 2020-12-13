module Kata.MyState where

newtype MyState s a = MyState { runMyState :: s -> (s, a) }

evalMyState :: MyState s a -> s -> a
evalMyState (MyState h) = snd . h

execMyState :: MyState s a -> s -> s
execMyState (MyState h) = fst . h

instance Functor (MyState s) where
    fmap f (MyState h) = MyState $ \s -> let (s', a) = h s
                                         in (s', f a)

instance Applicative (MyState s) where
    pure x = MyState (\s -> (s, x))
    (MyState ff) <*> (MyState fa) = MyState $ \s -> let (s', f) = ff s
                                                        (s'', a) = fa s'
                                                    in (s'', f a)  
{-  msf <*> msa = do
                    f <- msf
                    a <- msa
                    return $ f a
-}

instance Monad (MyState s) where
    return x = MyState (\s -> (s, x)) 
    (MyState h) >>= f' = MyState $ \s -> let (s', a) = h s
                                             (MyState g) = f' a    
                                         in g s' 

-- (s -> (s, ()) 

put :: s -> MyState s () 
put s = MyState $ \_ -> (s, ())

-- (s -> (s, s))
get :: MyState s s 
get = MyState $ \s -> (s, s)

modify :: (s -> s) -> MyState s ()
modify f = MyState $ \s -> (f s, ())

gets :: (s -> a) -> MyState s a
gets f = fmap f get


-- MyState (\s -> (n:s, n))

f :: Int -> MyState [Int] Int
f n = do
        l <- gets (n:)
        put l
        return n 

f'' :: Int -> MyState [Int] Int
f'' n = gets (n:) >>= put >> return n

f' :: Int -> MyState [Int] Int
f' n = do   
        modify (n:)
        return n 

someFunc :: IO ()
someFunc = print $ runMyState (fmap (2*) ((+) <$> f 3 <*> f' 1)) []

