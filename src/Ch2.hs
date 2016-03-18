doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x
-- Not sure if the ' is apprpriate.  it denotes strict (not lazy) and I am not sure if it is true or not in this instance
doubleSmallNumber2' x = (doubleSmallNumber x) + 1

lostNumbers = [4,8,15,16,23,42]

-- apends to end of list and is slow.  transverses left side one by one
wootAppend = ['w','o'] ++ "ot"
-- inserts into head and verry fast
wootCons = 'w' : ['o','o','t']
-- access the list by position, 0 based
wootAccess x = [1,2,3,5,7,11] !! x
listComparison1 = [3,2,1] > [2,1,0]  --true
listComparison2 = [3,2,1] > [3,2]  --true if lists are of unequal length then the longer list is automatically greater than the smaller one
listComparison3 = [2,2,1] > [3,2] -- false
listComparison4 = [3] > [] --true

testList = [9,8,7,6,5,4,3,2]
headTest = head testList
tailTest = tail testList
initTest = init testList
lastTest = last testList

nullCheck = null testList -- False
nullCheckEmpty = null [] -- True
rangeNum = [5..10]
rangeNumStep = [4,8..30] -- specify the first 2 values..upper bound
rangeNumDecrease = [20,19..1] -- must specify step for decreasing values
infinateSeqDec x = take x [20,19..]
rangeChar = ['a'..'z']
cycleLOL x = take x (cycle "LOL ") -- 11 outs "LOL LOL LOL"
repeatLOL x = take x (repeat "LOL") -- 2 outs ["LOL","LOL"]
replicateLOL x = replicate x "LOL" -- same as above
doubleList = [x * 2 | x <- [1..10]]
doubleListWithFilter = [x * 2 | x <- [1..10], x*2 >= 12]
listWithFilter = [x | x <- [50..1000], x `mod` 7 == 3]
boomBang xs = [if x <10 then "BOOM" else "BANG" | x <- xs, odd x]
multiFilter = [ x | x <- [10..20], x/=13, x/=15, x/=19]
multiList = [x+y | x <- [1..3], y <- [10,100,1000]]
multiListWithFilter = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
multiListWithFilterPosition = [ x*y+z | x <- [2,5,10], y <- [8,10,11], x*y > 50, z <- [10..12]] -- position on the right side does not matter
length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
testNestedList = [[1,3,5,2,3,1,2,4,5],[1..9],[1,2,4,1,6,3,1,3,2,6]]
nestedListFilter xxs = [[x | x <- xs, even x] | xs <- xxs]
fstTuple = fst (8,11) --first element of pair (2 lement tuple)
sndTuple = snd (8,11) --second element of pair
zipTuple = zip [5,10..] ["ninja","foo","man","choo"]

triples = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24] 