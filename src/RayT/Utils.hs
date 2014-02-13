module RayT.Utils
    ( (~=), (>~)
    , solveQ
    ) where

-- * helpers

solveQ :: Double -> Double -> Double -> [Double]
solveQ a b c
    | a ~= 0 && b ~= 0 = []
    | a ~= 0    = [negate c / b]
    | det < 0   = []
    | det ~= 0  = [negate b / a]
    | otherwise = [(negate b + sdet)/(2*a), (negate b - sdet)/(2*a)]
    where det  = b*b - 4*a*c
          sdet = sqrt det

infixr 5 ~=
(~=) :: (Ord a, Fractional a) => a -> a-> Bool
a ~= b
    | abs b < tolerance = abs a < tolerance
    | otherwise         = abs ((a-b)/b) < tolerance

infix 6 >~
(>~) :: (Ord a, Fractional a) => a -> a -> Bool
a >~ b = a > b && not (a ~= b)

tolerance :: Fractional a => a
tolerance = 0.000001    
