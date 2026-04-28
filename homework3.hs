-- Homework 03

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (foldM, guard)
import Data.List (permutations)
import Control.Monad.Writer



-- Task 1 ==
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)
-- a
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  neighbours <- Map.lookup pos maze
  Map.lookup dir neighbours

-- b
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start dirs =
  foldl step (Just start) dirs
  where
    step :: Maybe Pos -> Dir -> Maybe Pos
    step maybePos dir = do
      pos <- maybePos
      move maze pos dir

-- c
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = do
  positions <- foldM step [start] dirs
  return (reverse positions)
  where
    step :: [Pos] -> Dir -> Maybe [Pos]
    step path dir = do
      let current = head path
      next <- move maze current dir
      return (next : path)

-- Example Task 1
-- It is a square-like maze
exampleMaze :: Maze
exampleMaze = Map.fromList
  [ ((0, 0), Map.fromList [(E, (1, 0)), (S, (0, 1))])
  , ((1, 0), Map.fromList [(W, (0, 0)), (S, (1, 1))])
  , ((0, 1), Map.fromList [(N, (0, 0)), (E, (1, 1))])
  , ((1, 1), Map.fromList [(N, (1, 0)), (W, (0, 1))])
  ]



-- Task 2 ==
type Key = Map Char Char
-- Decrypt one string
-- If a character is missing the result is Nothing
decrypt :: Key -> String -> Maybe String
decrypt key str = traverse (`Map.lookup` key) str

-- Decrypt a list of words
-- If a word cannot be decoded the result is Nothing
decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key wordsList = traverse (decrypt key) wordsList

-- Example Task 2 ==
exampleKey :: Key
exampleKey = Map.fromList
  [ ('h', 'H')
  , ('e', 'E')
  , ('l', 'L')
  , ('o', 'O')
  , ('w', 'W')
  , ('r', 'R')
  , ('d', 'D')
  ]



-- Task 3 ==
type Guest = String
type Conflict = (Guest, Guest)
seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  perm <- permutations guests
  guard (all (not . conflict conflicts) (adjacencies perm))
  return perm
  where
    -- Create neighboring pairs from the permutation
    adjacencies :: [Guest] -> [(Guest, Guest)]
    adjacencies (x:xs) = zip (x:xs) (xs ++ [x])
    adjacencies [] = []

    -- Check if two guests are in conflict
    conflict :: [Conflict] -> (Guest, Guest) -> Bool
    conflict conflicts (g1, g2) = (g1, g2) `elem` conflicts || (g2, g1) `elem` conflicts

-- Example Task 3
exampleGuests :: [Guest]
exampleGuests = ["Alice", "Bob", "Charlie", "David"]

exampleConflicts :: [Conflict]
exampleConflicts = [("Alice", "Bob"), ("Charlie", "David")]



-- Task 4 ==
data Result a = Failure String | Success a [String]
instance Functor Result where
  fmap _ (Failure msg) = Failure msg
  fmap f (Success val warnings) = Success (f val) warnings
  
instance Show a => Show (Result a) where
  show (Failure msg) = "Failure: " ++ msg
  show (Success val warnings) = "Success: " ++ show val ++ ", Warnings: " ++ show warnings
  
instance Applicative Result where
  pure val = Success val []  -- No warnings by default
  
  (Failure msg) <*> _ = Failure msg
  _ <*> (Failure msg) = Failure msg
  (Success f warningsF) <*> (Success val warningsVal) = Success (f val) (warningsF ++ warningsVal)

instance Monad Result where
  Failure msg >>= _ = Failure msg

  Success val warnings >>= f =
    case f val of
      Failure msg ->
        Failure msg

      Success newVal newWarnings ->
        Success newVal (warnings ++ newWarnings)


-- Example Task 4
-- b
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure msg = Failure msg

-- c
validateAge :: Int -> Result Int
validateAge age
  | age < 0   = failure "Age cannot be negative"
  | age > 150 = warn "Age is greater than 150" >> return age
  | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = foldM (\acc age -> do
  validatedAge <- validateAge age
  return (acc ++ [validatedAge])) [] 




-- Task 5 ==
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
instance Show Expr where
  show (Lit n)      = show n
  show (Add e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Mul e1 e2)  = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Neg e)      = "(-" ++ show e ++ ")"
-- Writer monad for simplification log
simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)  -- No simplification needed for literals
simplify (Add (Lit 0) e) = do
  tell ["Add identity: 0 + e -> e"]
  simplify e
simplify (Add e (Lit 0)) = do
  tell ["Add identity: e + 0 -> e"]
  simplify e
simplify (Mul (Lit 1) e) = do
  tell ["Mul identity: 1 * e -> e"]
  simplify e
simplify (Mul e (Lit 1)) = do
  tell ["Mul identity: e * 1 -> e"]
  simplify e
simplify (Mul (Lit 0) _) = do
  tell ["Mul zero: 0 * e -> 0"]
  return (Lit 0)
simplify (Mul _ (Lit 0)) = do
  tell ["Mul zero: e * 0 -> 0"]
  return (Lit 0)
simplify (Mul (Lit a) (Lit b)) = do
  tell ["Mul constant: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a * b)]
  return (Lit (a * b))
simplify (Add (Lit a) (Lit b)) = do
  tell ["Add constant: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a + b)]
  return (Lit (a + b))
simplify (Neg (Neg e)) = do
  tell ["Double negation: --e -> e"]
  simplify e
simplify (Neg e) = do
  tell ["Negation: -e -> -e"]
  simplify e
simplify (Add e1 e2) = do
  e1' <- simplify e1
  e2' <- simplify e2
  return (Add e1' e2')
simplify (Mul e1 e2) = do
  e1' <- simplify e1
  e2' <- simplify e2
  return (Mul e1' e2')



main :: IO ()
main = do
  putStrLn "=== Homework 03 ==="
  
  putStrLn ""
  putStrLn "== Task 1 =="
  putStrLn ""
  putStrLn "-- a"
  print (move exampleMaze (0, 0) E)
  print (move exampleMaze (0, 0) W)

  putStrLn ""
  putStrLn "-- b"
  print (followPath exampleMaze (0, 0) [E, S])
  print (followPath exampleMaze (0, 0) [W, S])

  putStrLn ""
  putStrLn "-- c"
  print (safePath exampleMaze (0, 0) [E, S, W])
  print (safePath exampleMaze (0, 0) [E, W, W])



  putStrLn ""
  putStrLn "== Task 2 =="
  putStrLn ""
  print (decrypt exampleKey "hello")
  print (decrypt exampleKey "hellox")
  print (decryptWords exampleKey ["hello", "world"])
  print (decryptWords exampleKey ["hello", "world", "test"])



  putStrLn ""
  putStrLn "== Task 3 =="
  putStrLn ""
  let validSeatings = seatings exampleGuests exampleConflicts
  print validSeatings



  putStrLn ""
  putStrLn "== Task 4 =="
  putStrLn ""
  -- Test the validateAge function
  print (validateAge (-1))  -- Should fail with an error message
  print (validateAge 160)   -- Should succeed with a warning
  print (validateAge 25)    -- Should succeed without warnings

  -- Test the validateAges function
  print (validateAges [10, -1, 150, 200]) -- Should return a failure with an error message
  print (validateAges [10, 20, 30]) -- Should succeed with no warnings
  
  
  
  putStrLn ""
  putStrLn "== Task 5 =="
  putStrLn ""
  let expr = Add (Mul (Lit 1) (Lit 5)) (Add (Lit 0) (Lit 10))
  
  let (simplifiedExpr, log) = runWriter (simplify expr)
  
  putStrLn "Simplified Expression:"
  print simplifiedExpr
  
  putStrLn "\nSimplification Log:"
  mapM_ putStrLn log
  
