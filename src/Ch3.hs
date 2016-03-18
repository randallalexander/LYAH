factorial :: Integer -> Integer
factorial n = product [1..n]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
-- (type constraint) => (type definition)
-- :t (==) = (Eq a) => a -> a -> Bool -- Eq = same type
-- :t (>) = (Ord a) => a -> a -> Bool -- Ord = Ordinal type
comp1 = "Abrakadabra" < "Zebra"
comp2 = 5 `compare` 3
-- :t show = show :: Show a => a -> String
showEx1 = show 3 -- types that are intsatnces of the show type class are converted to Strings
-- :t read = read :: Read a => String -> a
-- note it needs a type so read "1" + 1 and read "1" :: Int is good but read "1" is not because the type can not be figured out
readEx1 = read "[1,2,3,4]" ++ [5] -- types that are instances of the read type class can be coverted from strings
-- Instances of boundType have minBound and maxBound
minInt = minBound :: Int
maxChar = maxBound :: Char
diffMaxes = maxBound :: (Bool, Int, Char)
-- Integral type class is an Int or Integer
-- :t fromIntegral = fromIntegral :: (Integral a, Num b) => a -> b
