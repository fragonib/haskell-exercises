{-# LANGUAGE FlexibleInstances #-}
module HaskellProgramming.Functor where
  
data Sum a b = 
    First a
  | Second b
  deriving (Eq, Show)

-- fmap :: (a -> b) -> f a -> f b
--instance Functor (Sum a) where
--   fmap f (First c) = First (f c) 
--   fmap _ (Second y) = Second y
   


-- Write functor instances for the following datatypes

-- Exercise 1
data Quant a b = 
    Finance 
  | Desk a 
  | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- Exercise 2
newtype K a b = 
  K a

instance Functor (K a) where
    fmap _ (K a) = K a

-- Exercise 3
newtype Flip f a b = 
  Flip (f b a) 
  deriving (Eq, Show)
  
instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip (K (f a))

instance Functor (Flip Either t) where
    fmap f (Flip (Left a)) = Flip (Left (f a))
    fmap _ (Flip (Right b)) = Flip (Right b)

-- Exercise 4
newtype EvilGoateeConst a b = 
  GoatyConst b    

-- Exercise 5
instance Functor (EvilGoateeConst t) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- Exercise 5
newtype LiftItOut f a = 
  LiftItOut (f a)

instance Functor t => Functor (LiftItOut t) where
    fmap f' (LiftItOut ta) = LiftItOut (fmap f' ta)
--instance Functor (LiftItOut Maybe) where    
--    fmap f' (LiftItOut a) = LiftItOut (fmap f' a)

-- Exercise 6
data Parappa f g a = 
  DaWrappa (f a) (g a)

instance (Functor s, Functor t) => Functor (Parappa s t) where
    fmap f' (DaWrappa sa ta) = DaWrappa (fmap f' sa) (fmap f' ta)

-- Exercise 7
data IgnoreOne f g a b = 
  IgnoringSomething (f a) (g b)

instance Functor t => Functor (IgnoreOne s t u) where
    fmap f' (IgnoringSomething sa ta) = IgnoringSomething sa (fmap f' ta)

-- Exercise 8
data Notorious g o a t = 
  Notorious (g o) (g a) (g t)

instance Functor s => Functor (Notorious s u v) where
    fmap f' (Notorious su sv sw) = Notorious su sv (fmap f' sw)

-- Exercise 9
data List a = Nil | Cons a (List a)

instance Functor List where
    fmap f' Nil = Nil
    fmap f' (Cons x y) = Cons (f' x) (fmap f' y)

-- Exercise 10
data GoatLord a =
    NoGoat | OneGoat a | 
    MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap f' NoGoat = NoGoat
    fmap f' (OneGoat x) = OneGoat (f' x)
    fmap f' (MoreGoats x y z) = MoreGoats (fmap f' x) (fmap f' y) (fmap f' z)

-- Exercise 11
data TalkToMe a = Halt | Print String a | Read (String -> a)    

instance Functor TalkToMe where
    fmap f' Halt = Halt
    fmap f' (Print x y) = Print x (f' y)
    --fmap f' (Read f) = Read (fmap f' f)
    fmap f' (Read f) = Read (f' . f)
