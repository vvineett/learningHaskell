import Data.List
import Data.Char
import Control.Monad

main = do
	--["000005900","070062001","019003000","020009003","000000000","700300010","000400280","300820060","008500000"]
	--["003020600","900305001","001806400","008102900","700000008","006708200","002609500","800203009","005010300"]
	contents <- readFile "./sudoku.txt" --["087000000","200000070","040006900","000630502","906502107","705089000","003200060","050000001","000000320"]
	--putStrLn $ show $ getPuzzles (lines contents)
	let puzzles = getPuzzles (lines contents) in putStrLn $ show $ sum([read (take 3 (head x))::Int| x<- (map (constructSol.solvePuzzle) puzzles)])
	--let puzzle  = ["200080300","060070084","030500209","000105408","000000000","402706000","301007040","720040060","004010003"] in putStrLn $ show $ (solvePuzzle) puzzle


type Puzzle = [[Char]]
type Pos = (Int, Int)
type Domain = [Char]
type State = [(Pos,Domain)]
type Assigned = [(Pos,Char)]


getPuzzles::[String]->[Puzzle]
getPuzzles c = map tail (group' 10 c)

group'::Int->[String]->[[String]]
group' _ [] = []
group' n li = (take 10 li):(group' n (drop n li))

constructSol::Maybe Assigned->Puzzle
constructSol (Just []) = (take 9 (repeat "000000000"))
constructSol Nothing = (take 9 (repeat "000000000"))
constructSol (Just (h:t)) = fill (fst h) (snd h) (constructSol (Just t)) where
	fill (i, j) e (h : t)	| i == 0	= (replace j e h) : t
							| i > 0		= h : (fill (i-1, j) e t)
							| i < 0		= h : t

--type Constraint = [(Pos,Pos)]

domainGen::Puzzle->State
domainGen puzzle = [((x,y), d) | x<-[0..8], y<-[0..8], let d = if (puzzle !! x)!!y /= '0' then ((puzzle !! x)!!y):[] else ['1'..'9']]

--constraintGraph::Constraint
--constraintGraph = [(p,q) | p<-[(x,y)|x<-[0..8],y<-[0..8]], q<-(conflictPos p)] 

consistent::State->Bool
consistent = foldl (\acc x -> (length (snd x) /= 0) && acc) True

--solved::Sate->Bool
--solved state = (length [ w | pd <- state, let w = (snd pd), length w == 1]) == 81

conflictPos::Pos->[Pos]
conflictPos pos = remDups ([(fst pos, y) | y<-[0..8],y/=(snd pos)] ++ [(x, snd pos) | x <- [0..8], x/= (fst pos)] ++ (boxConst pos)) where
	boxConst pos = [(x,y) | x<-[3*(boxNum `div` 3)..(3*(boxNum `div` 3)+2)], y<- [3*(boxNum `rem` 3)..(3*(boxNum `rem` 3)+2)], (x,y) /= pos] where
		boxNum = 3*((fst pos) `div` 3) + (snd pos) `div` 3

remDups::(Eq a)=>[a]->[a]
remDups (x:[]) =  x:[]
remDups (x:xs)
	|x `elem` xs = remDups xs
	|otherwise = x:remDups xs

nextState::State->Pos->Char->State
nextState state pos c = [ (p,d) | x<-state, let p = fst x; d = if p `elem` confP then (filter (c /= ) (snd x)) else snd x, p /= pos] where
	confP = conflictPos pos

checkAssignmentConflict::Assigned->Pos->Char->Bool
checkAssignmentConflict a pos c = sum [ 1 | p<- conflictPos pos, (p,c) `elem` a] == 0

assign::Assigned->Pos->Char->Assigned
assign a pos c = (pos,c):a

solvePuzzle::Puzzle->Maybe Assigned
solvePuzzle puzzle = start [] (domainGen puzzle)

start::Assigned->State->Maybe Assigned
start a state
	| complete = Just a
	| not (consistent state) = Nothing
	| otherwise = if (checkAssignmentConflict a p c)
				then if recurse == Nothing then start a (update state p c) else recurse
				else start a (update state p c) where
		recurse = (start (assign a p c) (nextState state p c))
		complete = (length a == 81)
		min = (foldl (\acc x -> if length (snd x) <= length (snd acc) then x else acc) ((0,0),['1'..'9']) state)
		p = fst min
		c = head (snd min)
		update state pos ch = [(position, d) | x<-state, let position = fst x; d = if position == pos then tail (snd x) else snd x]

replace::Int->a->[a]->[a]
replace _ _ [] = []
replace i e (h:t)	| i == 0 = e : t
					| i > 0  = h : (replace (i -1) e t)
					| i < 0  = h : t