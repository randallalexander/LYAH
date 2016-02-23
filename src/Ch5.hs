multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- applyTwice ("HAHA " ++) "HEY" -- "HAHA HAHA HEY"
-- applyTwice (++ " HAHA") "HEY" -- "HEY HAHA HAHA"
-- applyTwice (3:) [1]
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- zipWith' max [6,3,2,1] [7,3,1,5]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip' zip [1,2,3,4,5] "hello"
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x -- with lambda
-- flip' f y x = f x y --better
-- flip' f = g  --basic
--     where g x y = f y x

-- map (map (^2)) [[1,2],[3,4,5,6],[7,8]] 
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- map' _ [] = []
-- map' f (x:xs) = f x : map' f xs

-- filter (>3) [1,5,3,2,1,6,4,3,2,1]
-- filter even [1..10]
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)


numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
--numLongChains = length (filter isLong (map chain [1..100]))
--    where isLong xs = length xs > 15

-- let listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) 5
-- take 5 $ map (\x -> x 5) listOfFuns 

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0
-- sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- sum (filter (> 10) (map (*2) [2..10])
-- sum $ filter (> 10) $ map (*2) [2..10]

-- map ($ 3) [(4+), (10*), (^2), sqrt]

-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24] 

-- map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
-- map (negate . sum . tail) [[1..5],[3..6],[1..7]]

-- fn x = ceiling (negate (tan (cos (max 50 x)))) -- don't forget the let in ghci
-- fn = ceiling . negate . tan . cos . max 50 -- don't forget the let in ghci
-- ceiling . negate . tan . cos . max 50 $ 100
-- tan $ cos 50
