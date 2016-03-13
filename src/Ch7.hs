module Shapes   
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where  

import qualified Data.Map as Map

data MyBool = MyFalse | MyTrue

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- surface $ Circle 10 20 10
-- surface $ Rectangle 0 0 100 100

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- surface (Rectangle (Point 0 0) (Point 100 100))
-- surface (Circle (Point 0 0) 24)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- nudge (Circle (Point 34 34) 10) 5 10

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r
  
baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- nudge (baseRect 40 100) 60 23

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- Car {company="Ford", model="Mustang", year=1967}

data MyMaybe a = MyNothing | MyJust a --Similar to Scala Option

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


data Vector a = Vector a a a deriving (Show)
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
  
vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)
  
scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
-- Vector 3 9 7 `vectMult` 10
-- Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0
-- Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

-- let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
-- mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- let beastieBoys = [mca, adRock, mikeD]
-- mikeD `elem` beastieBoys

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Monday `compare` Wednesday
-- maxBound :: Day
-- pred Saturday
-- [Thursday .. Sunday]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
     [(100, (Taken, "ZD39I"))
     ,(101, (Free, "JAH3I"))
     ,(103, (Free, "IQSA9"))
     ,(105, (Free, "QOTSA"))
     ,(109, (Taken, "893JJ"))
     ,(110, (Taken, "99292"))
     ]

-- lockerLookup 101 lockers
-- lockerLookup 100 lockers

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- Red == Red
-- Red `elem` [Red, Yellow, Green]
-- [Red, Yellow, Green]

-- fmap (*2) (Just 200)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

-- fmap (*2) EmptyTree
-- fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])