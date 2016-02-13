sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe nope = "Blarg!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

tuple_xs = [(1, 2), (2, 3), (3, 4)]
addTuples :: (Num a) =>  [(a,a)] -> [a]
addTuples xs = [ a + b | (a, b) <- xs ]

head' :: [a] -> a
head' [] = error "can't take the head of an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty List"
tell (x:[]) = "List of 1:" ++ show x
tell (x:y:[]) = "List of 2:" ++ show x ++ ":" ++ show y
tell (x:y:xs) = "Long list:" ++ show x ++ ":" ++ show y ++ ": and " ++ show (length xs) ++ " more"

firstLetter :: String -> String
firstLetter "" = error "cant get the first letter of an empty string"
firstLetter all@(x:xs) = [x] ++ " is the first letter of " ++ all

bmiTell :: Double -> Double -> String
bmiTell weight height
 | bmi <= 18.5 = "Eat more food"
 | bmi <= 25.0 = "Looking good"
 | bmi <= 39.0 = "Need a workout"
 | otherwise = "Go see a doctor"
 where bmi = weight / height ^ 2

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
 where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

-- (let a = 100 ; b = 200 in a * b) + 1
-- (let (a,b,c) = (1,2,3) in a+b+c) * 100 
-- [let square x = x * x in (square 5, square 3, square 2)]

calcBmis2 :: [(Double, Double)] -> [Double]
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w /h ^ 2]

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 
