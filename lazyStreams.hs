fib :: (Integral a) => [a]
fib = 0:1: (zipWith (+) fib (tail fib))

sumEvenLessThan4M :: (Integral a) => [a] -> a
sumEvenLessThan4M = sum.filter even.takeWhile (< 4000000)

-- data Tree a b = Empty | Tree (Tree a b) b a b (Tree a b) deriving (Show, Eq, Read)

-- countNode :: Tree a (Maybe Integer) -> a -> Tree a Integer
-- countNode Empty _ = Empty
-- countNode (Tree left Nothing y Nothing right) x = Tree leftCountedTree (getCount leftCountedTree x) y (getCount rightCountedTree x) rightCountedTree)
--  where leftCountedTree = countNode left x
--      rightCountedTree = countNode right x
--      getCount Empty _ = 0
--      getCount (Tree l lCount z rCount r) elem    | z == elem = lCount + rCount + 1
--                                                  | otherwise = lCount + rCount

-- findPaths :: Tree a Integer ->  

-- First five Ramanujan Numbers

positiveIntegers :: (Integral a) => [a]
positiveIntegers = [1..]

pairs :: (Integral a) => ((a,a) -> a) -> [a] -> [a] -> [(a,a)]
pairs _ [] _ = []
pairs _ _ [] = []
pairs weight (x:xs) (y:ys) = (x, y) : (merge weight (map (\ z -> (x, z)) ys) (pairs weight xs ys))

merge :: (Num a, Ord a) => (b -> a) -> [b] -> [b] -> [b]
merge _ [] ys = ys
merge _ xs [] = xs
merge weight l1@(x:xs) l2@(y:ys)    | (weight x <= weight y) = x : (merge weight xs l2)
                                    | otherwise              = y : (merge weight l1 ys)

ramanujanNumbers :: (Integral a) => [a]
ramanujanNumbers = duplicatesOnly (map cubicWeight cubicWeightedPairs) where
    cubicWeightedPairs = pairs cubicWeight positiveIntegers positiveIntegers
    cubicWeight (x, y) = x^3 + y^3

-- works only for ordered streams
duplicatesOnly :: (Eq a) => [a] -> [a]
duplicatesOnly [] = []
duplicatesOnly (x:[]) = []
duplicatesOnly (x:xs) | x == head xs = x : duplicatesOnly ( dropWhile (== x) xs)
                    | otherwise = duplicatesOnly xs

