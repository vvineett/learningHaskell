import qualified Data.Map.Lazy as Map

main = do
	putStrLn $ show $ list12 where
		list12 = foldl add (0,Map.empty) [ (sProd w count (px 12 n)) | x <- Map.toList (snd list8), let w = (fst list8); count = snd x; n = fst x ] where
			list8 = foldl add (0,Map.empty) [ (sProd w count (px 8 n)) | x <- Map.toList (snd list6), let w = (fst list6); count = snd x; n = fst x ] where
				list6 = foldl add (0,Map.empty) [ (sProd w count (px 6 n)) | x <- Map.toList (snd list4), let w = (fst list4); count = snd x; n = fst x ] where
					list4 = px 4 1
	--putStrLn $ show $ foldl add (0,Map.empty) [ (sProd w count (px 20 n)) | x <- Map.toList (snd list12), let w = (fst list12); count = snd x; n = fst x ] where
	--	list12 = foldl add (0,Map.empty) [ (sProd w count (px 12 n)) | x <- Map.toList (snd list8), let w = (fst list8); count = snd x; n = fst x ] where
	--		list8 = foldl add (0,Map.empty) [ (sProd w count (px 8 n)) | x <- Map.toList (snd list6), let w = (fst list6); count = snd x; n = fst x ] where
	--			list6 = foldl add (0,Map.empty) [ (sProd w count (px 6 n)) | x <- Map.toList (snd list4), let w = (fst list4); count = snd x; n = fst x ] where
	--				list4 = px 4 1


type Weight = Integer
type Dist = (Weight,Map.Map Integer Integer)

px::Integer->Integer->Dist
px k 1 = (k, Map.fromList [(x,1) | x<-[1..k]]) 
px k n
	|even' = prod halfnP halfnP
	|otherwise = prod oneP (prod halfnP halfnP) where
		even' = even n
		halfnP = px k (n `div` 2)
		oneP = px k 1

prod::Dist->Dist->Dist
prod (w, m) b = a where
	a = Map.foldlWithKey (\acc k v -> add (oneProd k v (snd b)) acc) (0,Map.empty) m
	oneProd x count dMap = (wt, aMap) where
		aMap = (Map.foldlWithKey (\acc k v -> (Map.insertWith (+) (k+x) (count*v) acc)) Map.empty dMap)
		wt = Map.foldl (+) 0 aMap 

add::Dist->Dist->Dist
add (wX,mX) (wY,mY) = (wX+wY, (Map.unionWith (+) mX mY))

sProd::Weight->Integer->Dist->Dist
sProd w count (wt,m) = (w*wt, newM) where
	newM = Map.map (count*) m
