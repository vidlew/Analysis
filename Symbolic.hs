{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Symbolic where

infixl 5 :+
infixr 5 :-
infixl 7 :*
infixr 7 :/

data Expr a where
  Var :: String -> Expr a
  Const :: a -> Expr a
  (:+) :: (Num a) => Expr a -> Expr a -> Expr a
  (:*) :: (Num a) => Expr a -> Expr a -> Expr a
  (:-) :: (Num a) => Expr a -> Expr a -> Expr a
  (:/) :: (Fractional a) => Expr a -> Expr a -> Expr a 
  Pi :: (Floating a) => Expr a
  Exp :: (Floating a) => Expr a -> Expr a
  Log :: (Floating a) => Expr a -> Expr a
  Sin :: (Floating a) => Expr a -> Expr a
  Cos :: (Floating a) => Expr a -> Expr a
  Asin :: (Floating a) => Expr a -> Expr a
  Acos :: (Floating a) => Expr a -> Expr a
  Atan :: (Floating a) => Expr a -> Expr a
  Sinh :: (Floating a) => Expr a -> Expr a
  Cosh :: (Floating a) => Expr a -> Expr a
  Asinh :: (Floating a) => Expr a -> Expr a
  Acosh :: (Floating a) => Expr a -> Expr a
  Atanh :: (Floating a) => Expr a -> Expr a

deriving instance (Show a) => Show (Expr a)
deriving instance (Eq a) => Eq (Expr a)

instance (Num a) => Num (Expr a) where{
  fromInteger = Const . fromInteger
; (+) = (:+)
; (*) = (:*)
; (-) = (:-)
}

instance (Fractional a) => Fractional (Expr a) where{
  fromRational = Const . fromRational
; (/) = (:/)
}

instance (Floating a) => Floating (Expr a) where{
  pi = Pi
; exp = Exp
; log = Log
; sin = Sin
; cos = Cos
; asin = Asin
; acos = Acos
; atan = Atan
; sinh = Sinh
; cosh = Cosh
; asinh = Asinh
; acosh = Acosh
; atanh = Atanh
}

partial :: (Num a) => String -> Expr a -> Expr a
partial _ (Const x) = 0
partial s (Var t) = if s==t then 1 else 0
partial s (x:+y) = (partial s x) + (partial s y)
partial s (x:*y) = (partial s x)*y + x+(partial s y)
partial s (x:/y) = ((partial s x)*y - x*(partial s y)) / (y*y)
partial _ Pi = 0
partial s (Exp x) = (partial s x) * (Exp x)
partial s (Log x) = (partial s x) / x
partial s (Sin x) = (partial s x) * (Cos x)
partial s (Cos x) = -(partial s x) * (Sin x)
partial s (Asin x) = (partial s x) / (sqrt $ 1-x*x)
partial s (Acos x) = -(partial s x) / (sqrt $ 1-x*x)
partial s (Atan x) = (partial s x) / (1+x*x)
partial s (Sinh x) = (partial s x) * (Cosh x)
partial s (Cosh x) = (partial s x) * (Sinh x)
partial s (Asinh x) = (partial s x) * (sqrt $ x*x+1)
partial s (Acosh x) = (partial s x) * (sqrt $ x*x-1)
partial s (Atanh x) = -(partial s x) / (1-x*x)


reduce :: (Eq a) => Expr a -> Expr a
reduce ((Const 0):+x) = x
reduce (x:+(Const 0)) = x
reduce (x:-(Const 0)) = x
reduce ((Const 1):*x) = x
reduce (x:*(Const 1)) = x
reduce ((Const 0):*_) = 0
reduce (_:*(Const 0)) = 0
reduce ((Const x):+(Const y)) = Const $ x+y
reduce ((Const x):-(Const y)) = Const $ x-y
reduce ((Const x):*(Const y)) = Const $ x*y
reduce ((Const x):/(Const y)) = Const $ x/y
reduce ((x:+y):+z) = (reduce x):+(reduce $ y:+z)
reduce ((x:*y):*z) = (reduce x):*(reduce $ y:*z)
reduce (x:*(y:+z)) = (reduce $ x*y) + (reduce $ x*z)
reduce ((x:+y):*z) = (reduce $ x*z) + (reduce $ y*z)
reduce (x:+y) = (reduce x):+(reduce y)
reduce (x:*y) = (reduce x):*(reduce y)
reduce (x:-y) = (reduce x):-(reduce y)
--not finished
reduce x = x
