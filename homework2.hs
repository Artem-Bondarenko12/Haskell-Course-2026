module Main where

-- Task 1 ==
data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving Show

instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)



-- Task 2 ==
instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r
    

seqToList :: Sequence a -> [a]
seqToList = foldr (:) []

seqLength :: Sequence a -> Int
seqLength = length
    
    

-- Task 3 ==
instance Semigroup (Sequence a) where
    (<>) :: Sequence a -> Sequence a -> Sequence a
    (<>) = Append
instance Monoid (Sequence a) where
    mempty :: Sequence a
    mempty = Empty

    
    
-- Task 4 ==
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target seq0 = go [seq0]
  where
    go [] = False
    go (Empty : rest) = go rest
    go (Single x : rest)
      | x == target = True
      | otherwise   = go rest
    go (Append l r : rest) = go (l : r : rest)



-- Task 5 ==
tailToList :: Sequence a -> [a]
tailToList seq0 = reverse (go [seq0] [])
  where
    go [] acc = acc
    go (Empty : rest) acc = go rest acc
    go (Single x : rest) acc = go rest (x : acc)
    go (Append l r : rest) acc = go (l : r : rest) acc



-- Task 6 ==
data Token = TNum Int | TAdd | TSub | TMul | TDiv
tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing

    go (TNum n : ts) stack = go ts (n : stack)

    go (TAdd : ts) (x : y : stack) = go ts ((y + x) : stack)
    go (TSub : ts) (x : y : stack) = go ts ((y - x) : stack)
    go (TMul : ts) (x : y : stack) = go ts ((y * x) : stack)
    go (TDiv : ts) (0 : _ : _) = Nothing
    go (TDiv : ts) (x : y : stack) = go ts ((y `div` x) : stack)

    go (_ : _) _ = Nothing



-- Task 7 ==
-- a 
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


-- b 
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr step []
  where
    step x acc
        | p x       = x : acc
        | otherwise = []


-- c
decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0



-- Task 8 ==
-- a 
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)]
    step x ((y, n) : ys)
        | x == y    = (y, n + 1) : ys
        | otherwise = (x, 1) : (y, n) : ys


-- b 
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []




main :: IO ()
main = do
    putStrLn "=== Homework 02 ==="
    putStrLn ""


    putStrLn "== Task 1 =="
    putStrLn ""
    let seq1 :: Sequence Int
        seq1 = Append (Single 1) (Append (Single 2) (Single 3))

        result :: Sequence Int
        result = fmap (*2) seq1

    print result



    putStrLn ""
    putStrLn "== Task 2 =="
    putStrLn ""
    print (seqToList seq1)
    print (seqLength seq1)
    
    
    
    putStrLn ""
    putStrLn "== Task 3 =="
    putStrLn ""
    let s1 = Append (Single 1) (Single 2)
        s2 = Append (Single 3) (Single 4)
    
    print (seqToList (s1 <> s2))
    print (seqToList (mempty <> s1))
    
    
    
    putStrLn ""
    putStrLn "== Task 4 =="
    putStrLn ""
    let s = Append (Single 1) (Append (Single 2) (Single 3))
    
    print (tailElem 2 s)  -- True
    print (tailElem 4 s)  -- False



    putStrLn ""
    putStrLn "== Task 5 =="
    putStrLn ""
    let s = Append (Single 1) (Append (Single 2) (Single 3))
    
    print (tailToList s)



    putStrLn ""
    putStrLn "== Task 6 =="
    putStrLn ""
    print (tailRPN [TNum 2, TNum 3, TAdd])
    print (tailRPN [TNum 10, TNum 2, TSub])
    print (tailRPN [TNum 6, TNum 3, TDiv])
    print (tailRPN [TNum 1, TAdd])
    print (tailRPN [TNum 4, TNum 0, TDiv])
    print (tailRPN [TNum 2, TNum 3])
    
    
    
    putStrLn ""
    putStrLn "== Task 7 =="
    putStrLn ""
-- a     
    print(myReverse[1,2,3,4])
-- b   
    print(myTakeWhile even [10,2,3,4])
-- c 
    print(decimal [1,2,3,4])
  
    
    
    putStrLn ""
    putStrLn "== Task 8 =="
    putStrLn ""
  -- a     
    print (encode "aaabccca")

  -- b       
    print (decode [('a',3),('b',1),('c',3),('a',1)])
    
    
    
    