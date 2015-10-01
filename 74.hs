import Data.Char
import Data.List
main = do
	putStrLn $ show $ length $ filter (\x -> length x == 60) (nextInChain 1 [ x:[] | x<-[1..1000000]])

nextInChain depth chains
	| depth > 62 = chains
	| otherwise  = nextInChain (depth + 1) (map (\(h:t) -> if (nextElem h `elem` (h:t)) then (h:t) else (nextElem h):h:t) chains) where
		nextElem x = sum (map (\n -> factorial n) (map (Data.Char.digitToInt) (show x))) where
			factorial 0 = 1
			factorial n = n*factorial (n-1)