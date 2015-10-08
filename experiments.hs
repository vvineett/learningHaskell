import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
--main = do
	--putStrLn $ show $ unusualPrimesSeq

disregard::[Char]->[Char]
disregard [] = []
disregard (x:xs)
	|quote = disregard xs
	|otherwise = x: disregard xs where
		quote  = x == '"'

split::Char->[Char]->[[Char]]->[[Char]]
split _ [] acc =  acc
split s (x:xs) acc 
	|splitter = split s xs ("":acc)
	|otherwise = split s xs ((x:(head acc)):(tail acc)) where
		splitter = x==s

cubeRoot:: Float->Float
cubeRoot x = try 1 where
	try guess
		| goodguess = guess
		| otherwise = try improvedguess where
			goodguess     = abs(guess*guess - x) < 0.001
			improvedguess = avg guess (x/guess) where
				avg a b = (a+b)/2


type List a = [a]

name::Either Char Int
name = if 2>3 then Left 'V' else Right 4


class Any a where
	any' :: a->Bool

instance Any Integer where
	any' 0 = False
	any' _ = True


sumCurious::Int
sumCurious = sum [x | x<-[3..99999], x == sum (map (\n -> product[1..n]) (map (Data.Char.digitToInt) (show x)))]

numTriangleWords::[[Char]]->Int
numTriangleWords [] = 0
numTriangleWords (x:xs)
	|(isTriangleNum (num x)) = 1+ numTriangleWords xs
	|otherwise = numTriangleWords xs where
		num [] = 0
		num (w:wd) = toInt (Data.List.elemIndex w ['A'..'Z']) + num wd

		toInt::Maybe Int->Int
		toInt (Just n) = n+1
		toInt Nothing = 0

		isTriangleNum n = root == fromIntegral (round root) where
				root = (-1 + sqrt (1+8* fromIntegral n))

unusualPrimesSeq::[(Int,Int,Int)]
unusualPrimesSeq = [ (u,v,w) | u<-primes, v<- filter (>u) primes, w<- filter (<u) primes, 2*u == v+w, qsort (show u) == qsort (show v), qsort (show v) == qsort (show w)] where
	primes = [x | x <- seivedPrimes [2..9999], x>999]

--seivedPrimes::[Integral a]->[Integral a]
seivedPrimes [] = []
seivedPrimes (h:t) = h:(seivedPrimes [x | x <- t, (x `rem` h) /= 0])

qsort [] = []
qsort (h:t) = qsort [x | x<- t, x>=h] ++ h:[] ++ qsort [x | x<-t, x<h]

instance Functor Map.Map k where
	fmap f Map.empty = Map.empty
	fmap f (Map.fromList (h:t)) = Map.insert (fst h) (f (snd h)) (fmap f (Map.fromList t)) 