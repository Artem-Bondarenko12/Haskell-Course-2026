--TASK 1
--goldbachPairs :: Int -> [(Int, Int)]
--goldbachPairs n
--  | n < 4 || odd n = []
--  | otherwise =
--      [ (p, q)
--      | p <- [2 .. n `div` 2]
--      , let q = n - p
--      , isPrime p
--      , isPrime q
--      ]

--isPrime :: Int -> Bool
--isPrime x
--  | x < 2 = False
--  | x == 2 = True
--  | even x = False
--  | otherwise = null [ d | d <- [3,5 .. limit], x `mod` d == 0 ]
--  where
--    limit = floor (sqrt (fromIntegral x))



--TASK 2
--coprimePairs :: [Int] -> [(Int, Int)]
--coprimePairs xs =
--  [ (x, y)
--  | (i, x) <- zip [0..] xs
--  , y <- drop (i + 1) xs
--  , x < y
--  , gcd x y == 1
--  ]



--TASK 3
--sieve :: [Int] -> [Int]
--sieve [] = []
--sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

--primesTo :: Int -> [Int]
--primesTo n = sieve [2..n]

--isPrime :: Int -> Bool
--isPrime n
--  | n < 2 = False
--  | otherwise = n `elem` primesTo n



--TASK 4
--matMul :: [[Int]] -> [[Int]] -> [[Int]]
--matMul a b =
--  [ [ sum [ a !! i !! k * b !! k !! j | k <- [0 .. p - 1] ]
--    | j <- [0 .. n - 1]
--    ]
--  | i <- [0 .. m - 1]
--  ]
--  where
--    m = length a
--    p = length (head a)
--    n = length (head b)



--TASK 5
--permutations :: Int -> [a] -> [[a]]
--permutations 0 _  = [[]]
--permutations _ [] = []
--permutations k xs
--  | k < 0     = []
--  | otherwise =
--      [ y : zs
--      | (y, ys) <- select xs
--      , zs <- permutations (k - 1) ys
--      ]

--select :: [a] -> [(a, [a])]
--select [] = []
--select (x:xs) =
--  (x, xs) : [ (y, x:ys) | (y, ys) <- select xs ]



--TASK 6
--merge :: Ord a => [a] -> [a] -> [a]
--merge xs [] = xs
--merge [] ys = ys
--merge (x:xs) (y:ys)
--  | x < y     = x : merge xs (y:ys)
--  | x > y     = y : merge (x:xs) ys
--  | otherwise = x : merge xs ys

--hamming :: [Integer]
--hamming = 1 : merge (map (2*) hamming)
--                    (merge (map (3*) hamming)
--                           (map (5*) hamming))



--TASK 7
--power :: Int -> Int -> Int
--power b e = helper 1 b e

--helper :: Int -> Int -> Int -> Int
--helper !acc _ 0 = acc
--helper !acc b e = helper (acc * b) b (e - 1)



--TASK 8 
--listMaxSeq :: [Int] -> Int
--listMaxSeq (x:xs) = go x xs
--  where
--    go acc [] = acc
--    go acc (y:ys) =
--      let newAcc = max acc y
--      in newAcc `seq` go newAcc ys


--listMaxBang :: [Int] -> Int
--listMaxBang (x:xs) = go x xs
--  where
--    go !acc [] = acc
--    go !acc (y:ys) = go (max acc y) ys



--TASK 9 
--sieve :: [Int] -> [Int]
--sieve [] = []
--sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

--primes :: [Int]
--primes = sieve [2..]

--isPrime :: Int -> Bool
--isPrime n
--  | n < 2     = False
--  | otherwise = n `elem` takeWhile (<= n) primes



--TASK 10
--no strictness annotations
mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (s, n) = go 0 0 xs

    go :: Double -> Int -> [Double] -> (Double, Int)
    go s n []     = (s, n)
    go s n (x:xs) = go (s + x) (n + 1) xs


--strict version with bang patterns
meanStrict :: [Double] -> Double
meanStrict xs = s / fromIntegral n
  where
    (s, n) = go 0 0 xs

    go :: Double -> Int -> [Double] -> (Double, Int)
    go !s !n []     = (s, n)
    go !s !n (x:xs) = go (s + x) (n + 1) xs


--mean and variance
meanVariance :: [Double] -> (Double, Double)
meanVariance xs = (mu, var)
  where
    (s, ss, n) = go 0 0 0 xs

    mu  = s / fromIntegral n
    var = ss / fromIntegral n - mu * mu

    go :: Double -> Double -> Int -> [Double] -> (Double, Double, Int)
    go !s !ss !n []     = (s, ss, n)
    go !s !ss !n (x:xs) = go (s + x) (ss + x * x) (n + 1) xs




main :: IO ()

--TASK 1
--main = do
  --print (goldbachPairs 10)
  --print (goldbachPairs 26)
  
  
  
--TASK 2
--main = print (coprimePairs [2,3,4,5,6])
  
  
  
--TASK 3
--main = do
--  print (primesTo 30)
--  print (isPrime 29)
--  print (isPrime 30)



--TASK 4 
--main = do
--  let a = [[1,2,3],
--           [4,5,6]]

--  let b = [[7,8],
--           [9,10],
--           [11,12]]

--  print (matMul a b)



--TASK 5
--main = do
--  print (permutations 2 [1,2,3])



--TASK 6
--main = print (take 20 hamming)



--TASK 7
--main = do
--  print (power 2 6)
--  print (power 3 2)
--  print (power 7 0)



--TASK 8
--main = do
--  print (listMaxSeq [3,1,9,2,7])
--  print (listMaxBang [3,1,9,2,7])



--TASK 9
--main = do
--  print (take 10 primes)
--  print (isPrime 29)
--  print (isPrime 30)



--TASK 10
main = do
  print (mean [1,2,3,4])
  print (meanStrict [1,2,3,4])
  print (meanVariance [1,2,3,4])




