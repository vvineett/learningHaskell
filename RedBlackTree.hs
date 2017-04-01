-- An implementation of Red-Black Trees in Functional Setting (1993) by Chris Okasaki
-- Two invariants a Red-Black Tree follows to self balance:
-- 1. No red node has a red parent
-- 2. Every empty node has same number of black nodes on the path starting at root

data Color = Red | Black deriving Show

data Tree a = Empty | Tree Color (Tree a) a (Tree a) deriving Show

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

isMemberOf :: Ord a => a -> Tree a -> Bool
isMemberOf _ Empty = False
isMemberOf x (Tree _ l y r) | x > y = isMemberOf x r
                            | x == y = True
                            | x < y = isMemberOf x l


insert :: Ord a => a -> Tree a -> Tree a
insert x tree = makeBlack (ins tree)
    where   ins Empty = Tree Red Empty x Empty
            ins (Tree color l y r)  | x < y  = balance (Tree color (ins l) y r)
                                    | x == y = Tree color l y r
                                    | x > y  = balance (Tree color l y (ins r))
            makeBlack (Tree _ l y r) = Tree Black l y r


balance :: (Ord a) => Tree a -> Tree a
balance (Tree Black (Tree Red (Tree Red a x b) y c) z d) = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance (Tree Black (Tree Red a x (Tree Red b y c)) z d) = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance (Tree Black a x (Tree Red b y (Tree Red c z d))) = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance (Tree Black a x (Tree Red (Tree Red b y c) z d)) = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance tree = tree