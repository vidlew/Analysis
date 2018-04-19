{-
Automatic differentiation for functions defined on numeric types.
Guaranteed (?) to give the same result as symbolically differentiating and then evaluating (without simplifying expressions like sin(asin x)). It shouldn't magnify floating-point errors, but floating-point arithmetic is still janky af and should never be used by anyone for anything that matters. I mean, floating-point addition isn't even associative, ffs.
-}

{-#LANGUAGE Rank2Types#-}

module Derivative (rightDerivative, leftDerivative, derivative, newt{-, complexDerivative-}) where

--import Data.Complex

data Dual a = Dual a a deriving (Show, Eq)

instance (Num a, Eq a) => Num (Dual a) where{
  (Dual x x') + (Dual y y') = Dual (x+y) (x'+y')
; (Dual x x') * (Dual y y') = Dual (x*y) (x'*y+x*y')
; negate (Dual x x')        = Dual (negate x) (negate x')
; abs (Dual x x')           = if x == 0 then Dual 0 $ abs x' else Dual (abs x) $ (signum x)*x'
; fromInteger n             = Dual (fromInteger n) 0
; signum (Dual x x')        = if x == 0 then Dual (signum x') 0 else Dual (signum x) 0
}

instance (Num a, Enum a) => Enum (Dual a) where{
  toEnum x            = Dual (toEnum x) 0
; fromEnum (Dual x _) = fromEnum x
}

instance (Integral a, Enum a) => Integral (Dual a) where{
  toInteger (Dual x _)            = toInteger x
--quotRem/divMod left unimplemented
}


instance (Real a) => Real (Dual a) where{
  toRational (Dual x _) = toRational x
}

instance (RealFrac a) => RealFrac (Dual a) where{
  properFraction (Dual x x') = (\(p,q) -> if q == 0 && (fromIntegral $ signum p)*x'<0 then (p-(fromIntegral $ signum p), Dual (q+(fromIntegral $ signum p)) x') else (p, Dual q x')) $ properFraction x
}

--I have no idea what any of these functions are supposed to do.
instance (RealFloat a) => RealFloat (Dual a) where{
  decodeFloat (Dual x _)     = decodeFloat x
; encodeFloat a b            = Dual (encodeFloat a b) 0
; floatDigits (Dual x _)     = floatDigits x
; floatRadix (Dual x _)      = floatRadix x
; floatRange (Dual x _)      = floatRange x
; isDenormalized (Dual x x') = (isDenormalized x) || (isDenormalized x')
; isNaN (Dual x _ )          = isNaN x
; isInfinite (Dual x _)      = isInfinite x
; isIEEE (Dual x _)          = isIEEE x
; isNegativeZero (Dual x x') = x==0 && (isNegativeZero x')
}


instance (Fractional a, Eq a) => Fractional (Dual a) where{
  recip (Dual x x') = Dual (recip x) $ negate $ x'/(x*x)
; fromRational x    = Dual (fromRational x) 0
}

instance (Ord a) => Ord (Dual a) where{
  (Dual x x') <= (Dual y y') = x<y || (x==y && x'<=y')
}

instance (Floating a, Eq a) => Floating (Dual a) where{
  pi = Dual pi 0
; exp (Dual x x')   = Dual (exp x) $ (exp x)*x'
; log (Dual x x')   = Dual (log x) $ x'/x
; sin (Dual x x')   = Dual (sin x) $ (cos x)*x'
; cos (Dual x x')   = Dual (cos x) $ negate $ (sin x)*x'
; asin (Dual x x')  = Dual (asin x) $ x'/(sqrt $ 1-x*x)
; acos (Dual x x')  = Dual (acos x) $ negate $ x'/(sqrt $ 1-x*x)
; atan (Dual x x')  = Dual (atan x) $ x'/(1+x*x)
; sinh (Dual x x')  = Dual (sinh x) $ (cosh x)*x'
; cosh (Dual x x')  = Dual (cosh x) $ (sinh x)*x'
; asinh (Dual x x') = Dual (asinh x) $ x'/(sqrt $ 1+x*x)
; acosh (Dual x x') = Dual (acosh x) $ x'/(sqrt $ x*x-1)
; atanh (Dual x x') = Dual (atanh x) $ x'/(1-x*x)
; sqrt (Dual 0 x')  = if sqrt x' == sqrt x' then Dual 0 $ x'/0 else Dual (sqrt x') (sqrt x') --Square root of a negative infinitesimal should be NaN
; sqrt (Dual x x')  = Dual (sqrt x) $ x'/(2*sqrt x)
}

rightLimit :: (Num a, Num b) => ((Dual a) -> (Dual b)) -> a -> b
rightLimit f x = (\(Dual y _) -> y) $ f $ Dual x 1

leftLimit :: (Num a, Num b) => ((Dual a) -> (Dual b)) -> a -> b
leftLimit f x = (\(Dual y _) -> y) $ f $ Dual x $ -1

limit :: (Num a, Num b, Eq b) => ((Dual a) -> (Dual b)) -> a -> b
limit f x = if leftLimit f x == rightLimit f x then rightLimit f x else error "Limit does not exist"

isRightContinuous :: (Num a, Num b, Eq b) => ((Dual a) -> (Dual b)) -> a -> Bool
isRightContinuous f x = ((\(Dual y _) -> Dual y 0) $ f (Dual x 0)) == Dual (rightLimit f x) 0

isLeftContinuous :: (Num a, Num b, Eq b) => ((Dual a) -> (Dual b)) -> a -> Bool
isLeftContinuous f x = ((\(Dual y _) -> Dual y 0) $ f (Dual x 0)) == Dual (leftLimit f x) 0

isContinuous :: (Num a, Num b, Eq b) => ((Dual a) -> (Dual b)) -> a -> Bool
isContinuous f x = (isLeftContinuous f x) && (isRightContinuous f x)

rightDerivative :: (Num a, Num b, Eq b) => ((Dual a) -> (Dual b)) -> a -> b
rightDerivative f x = if isRightContinuous f x then (\(Dual _ e) -> e) $ f $ Dual x 1 else error "Not right continuous"

leftDerivative :: (Num a, Num b, Eq b) => ((Dual a) -> (Dual b)) -> a -> b
leftDerivative f x = if isLeftContinuous f x then (\(Dual _ e) -> -e) $ f $ Dual x $ -1 else error "Not left continuous"

derivative :: (Num a, Num b, Eq b) => ((Dual a) -> (Dual b)) -> a -> b
derivative f x = if isContinuous f x then if leftDerivative f x == rightDerivative f x then rightDerivative f x else error "Not differentiable" else error "Not continuous"

{-
re :: (Dual (Complex a)) -> Dual a
re (Dual x x') = Dual (realPart x) (realPart x')

im :: (Dual (Complex a)) -> Dual a
im (Dual x x') = Dual (imagPart x) (imagPart x')

transpose :: (Complex (Dual a)) -> (Dual (Complex a))
transpose z = let (Dual x x') = realPart z
                  (Dual y y') = imagPart z
              in Dual (x:+y) (x':+y')

transpose' :: (Dual (Complex a)) -> (Complex (Dual a))
transpose' (Dual z z') = (Dual (realPart z) (realPart z')) :+ (Dual (imagPart z) (imagPart z'))

complexDerivative' :: (Num a, Num b, Eq b) => ((Dual (Complex a)) -> (Dual (Complex b))) -> (Complex a) -> (Complex b)
complexDerivative' f z = let u   = re.f
                             v   = im.f
                             x   = realPart z
                             y   = imagPart z
                             u_x = derivative (u.(\(Dual s s') -> Dual (s:+y) $ s':+0)) x
                             v_x = derivative (v.(\(Dual s s') -> Dual (s:+y) $ s':+0)) x
                             u_y = derivative (u.(\(Dual s s') -> Dual (x:+s) $ 0:+s')) y
                             v_y = derivative (v.(\(Dual s s') -> Dual (x:+s) $ 0:+s')) y
                         in if (u_x == v_y) && (u_y == -v_x) then u_x :+ v_x else error $ "Not differentiable"

complexDerivative :: (Num a, Num b, Eq b) => ((Complex (Dual a)) -> (Complex (Dual b))) -> (Complex a) -> (Complex b)
complexDerivative f z = let u   = realPart.f
                            v   = imagPart.f
                            x   = realPart z
                            y   = imagPart z
                            u_x = derivative (u.(:+(Dual y 0))) x
                            v_x = derivative (v.(:+(Dual y 0))) x
                            u_y = derivative (u.((Dual x 0):+)) y
                            v_y = derivative (v.((Dual x 0):+)) y
                        in if (u_x == v_y) && (u_y == -v_x) then u_x :+ v_x else error "Not differentiable"
-}

--Use Newton's method to find a zero of f near x
--Returns a list of approximations
--f must be a polymorphic function defined for all floating types
newt :: (Floating a, Eq a) => (forall α. (Floating α) => α -> α) -> a -> [a]
newt f x = iterate (\ξ -> if f ξ == 0 then ξ else ξ - (f ξ)/(derivative f ξ)) x
