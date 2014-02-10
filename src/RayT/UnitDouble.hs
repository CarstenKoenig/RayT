-- | provides a datatype `UnitDouble` that are doubles bound between 0 and 1
module RayT.UnitDouble
    ( UnitDouble
    , unitD
    , valueD
    ) where

newtype UnitDouble = UD Double
    deriving Show

-- * operators

unitD :: Double => UnitDouble
unitD a
    | a < 0     = UD 0
    | a > 1     = UD 1
    | otherwise = UD a 

valueD :: UnitDouble => Double
valueD (UD a) = a

instance Eq UnitDouble where
    (UD a) == (UD b) = a == b

instance Ord UnitDouble where
    compare (UD a) (UD b) = compare a b

instance Fractional UnitDouble where
    fromRational r = UD $ fromRational r
    recip (UD a) = unitD $ recip a

-- | defining some basic operations on the bounded Doubles
-- REMARK: please note, that this is a bit of missuese as the defined
-- operations are not distributive: 
-- 0.5*(1+0.5) == 0.5*1 = 0.5 but  0.5*1 + 0.5*0.5 = 0.5+0.25 = 0.75
instance Num UnitDouble where
    (UD a) + (UD b) = unitD (a+b)
    (UD a) * (UD b) = unitD (a*b)
    (UD a) - (UD b) = unitD (a-b)
    negate (UD a)   = unitD (negate a)
    fromInteger a   = unitD (fromInteger a)
    abs (UD a)      = unitD (abs a)
    signum (UD a)   = unitD (signum a)


-- * helpers
tolerance :: Double
tolerance = 0.00001