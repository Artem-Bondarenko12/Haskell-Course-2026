data Regex
  = Char Char
  | Concat Regex Regex
  | Union Regex Regex
  | Star Regex
  | Empty
  deriving (Eq, Ord)

instance Show Regex where
  show = prettyRegex

prettyRegex :: Regex -> String
prettyRegex = render 0
  where
    render :: Int -> Regex -> String
    render contextPrecedence regex =
      let body =
            case regex of
              Empty      -> "ε"
              Char c     -> escapeLiteral c
              Star r     -> render 3 r ++ "*"
              Concat a b -> render 2 a ++ render 2 b
              Union a b  -> render 1 a ++ "|" ++ render 1 b
       in if precedence regex < contextPrecedence
            then "(" ++ body ++ ")"
            else body

    precedence :: Regex -> Int
    precedence regex =
      case regex of
        Union _ _  -> 1
        Concat _ _ -> 2
        Star _     -> 3
        Char _     -> 4
        Empty      -> 4

    escapeLiteral :: Char -> String
    escapeLiteral c
      | c `elem` specialChars = ['\\', c]
      | otherwise             = [c]

    specialChars :: [Char]
    specialChars = "|*()\\"

-- Parser

data ParseError = ParseError
  { errorOffset  :: Int
  , errorColumn  :: Int
  , errorMessage :: String
  } deriving (Eq)

instance Show ParseError where
  show parseError =
    "parse error at column "
      ++ show (errorColumn parseError)
      ++ ": "
      ++ errorMessage parseError

data Cursor = Cursor
  { remaining :: String
  , offset    :: Int
  } deriving (Eq, Show)

type Result a = Either ParseError (a, Cursor)

parseRegex :: String -> Either ParseError Regex
parseRegex input =
  case parseAlternation (Cursor input 0) of
    Left err -> Left err
    Right (regex, cursor) ->
      case peek cursor of
        Nothing  -> Right regex
        Just ')' -> parseError cursor "unexpected ')'; there is no matching '('"
        Just c   -> parseError cursor ("unexpected character " ++ show c)

parseOrThrow :: String -> Regex
parseOrThrow input =
  case parseRegex input of
    Right regex -> regex
    Left err    -> error (show err)

parseAlternation :: Cursor -> Result Regex
parseAlternation cursor = do
  (left, cursorAfterLeft) <- parseConcatenation cursor
  parseAlternationTail left cursorAfterLeft

parseAlternationTail :: Regex -> Cursor -> Result Regex
parseAlternationTail left cursor =
  case peek cursor of
    Just '|' -> do
      let cursorAfterPipe = advance cursor
      (right, cursorAfterRight) <- parseConcatenation cursorAfterPipe
      parseAlternationTail (Union left right) cursorAfterRight
    _ -> Right (left, cursor)

parseConcatenation :: Cursor -> Result Regex
parseConcatenation = go []
  where
    go :: [Regex] -> Cursor -> Result Regex
    go parts cursor =
      case peek cursor of
        Nothing  -> Right (combine parts, cursor)
        Just ')' -> Right (combine parts, cursor)
        Just '|' -> Right (combine parts, cursor)
        Just '*' -> parseError cursor "dangling '*'; there is no expression to repeat"
        _ -> do
          (part, cursorAfterPart) <- parseRepetition cursor
          go (parts ++ [part]) cursorAfterPart

    combine :: [Regex] -> Regex
    combine []       = Empty
    combine [single] = single
    combine (x : xs) = foldl Concat x xs

parseRepetition :: Cursor -> Result Regex
parseRepetition cursor = do
  (atom, cursorAfterAtom) <- parseAtom cursor
  parseStars atom cursorAfterAtom

parseStars :: Regex -> Cursor -> Result Regex
parseStars regex cursor =
  case peek cursor of
    Just '*' -> parseStars (Star regex) (advance cursor)
    _        -> Right (regex, cursor)

parseAtom :: Cursor -> Result Regex
parseAtom cursor =
  case peek cursor of
    Nothing   -> Right (Empty, cursor)
    Just '('  -> parseGroup cursor
    Just ')'  -> parseError cursor "unexpected ')'; there is no matching '('"
    Just '|'  -> parseError cursor "unexpected '|'; expected an expression"
    Just '*'  -> parseError cursor "dangling '*'; there is no expression to repeat"
    Just '\\' -> parseEscapedLiteral cursor
    Just 'ε'  -> Right (Empty, advance cursor)
    Just c    -> Right (Char c, advance cursor)

parseGroup :: Cursor -> Result Regex
parseGroup cursor = do
  let openParenOffset = offset cursor
      cursorAfterOpenParen = advance cursor
  (inside, cursorAfterInside) <- parseAlternation cursorAfterOpenParen
  case peek cursorAfterInside of
    Just ')' -> Right (inside, advance cursorAfterInside)
    Nothing  -> Left (ParseError openParenOffset (openParenOffset + 1) "mismatched '('; expected ')' before end of input")
    Just c   -> parseError cursorAfterInside ("unexpected character inside group: " ++ show c)

parseEscapedLiteral :: Cursor -> Result Regex
parseEscapedLiteral cursor =
  let slashOffset = offset cursor
      cursorAfterSlash = advance cursor
   in case peek cursorAfterSlash of
        Nothing -> Left (ParseError slashOffset (slashOffset + 1) "trailing backslash escape")
        Just c  -> Right (Char c, advance cursorAfterSlash)

peek :: Cursor -> Maybe Char
peek cursor =
  case remaining cursor of
    []      -> Nothing
    (c : _) -> Just c

advance :: Cursor -> Cursor
advance cursor =
  case remaining cursor of
    []       -> cursor
    (_ : cs) -> Cursor cs (offset cursor + 1)

parseError :: Cursor -> String -> Either ParseError a
parseError cursor message =
  Left (ParseError (offset cursor) (offset cursor + 1) message)

-- Small test runner

main :: IO ()
main = do
  putStrLn "Demo parses:"
  mapM_ printParsed ["", "a", "ab", "a|bc", "(a|b)*c", "a\\*b", "*a", "(ab"]
  putStrLn ""
  putStrLn "Running parser tests..."
  runTests
  putStrLn "All parser tests passed."

printParsed :: String -> IO ()
printParsed input =
  putStrLn (show input ++ "  =>  " ++ either show show (parseRegex input))

runTests :: IO ()
runTests = do
  expectParse "empty input" "" Empty
  expectParse "single literal" "a" (Char 'a')
  expectParse "explicit epsilon" "ε" Empty
  expectParse "concatenation" "ab" (Concat (Char 'a') (Char 'b'))
  expectParse "star binds before concat" "ab*" (Concat (Char 'a') (Star (Char 'b')))
  expectParse "concat binds before union" "a|bc" (Union (Char 'a') (Concat (Char 'b') (Char 'c')))
  expectParse "parenthesized union before star" "(a|b)*c" (Concat (Star (Union (Char 'a') (Char 'b'))) (Char 'c'))
  expectParse "escaped metacharacter" "a\\*b" (Concat (Concat (Char 'a') (Char '*')) (Char 'b'))
  expectParse "empty group" "()" Empty
  expectParse "empty alternative on right" "a|" (Union (Char 'a') Empty)
  expectParse "empty alternative on left" "|a" (Union Empty (Char 'a'))

  expectPretty "pretty adds required parens" (Concat (Star (Union (Char 'a') (Char 'b'))) (Char 'c')) "(a|b)*c"
  expectError "dangling star" "*a" 1
  expectError "mismatched paren" "(ab" 1
  expectError "unmatched close paren" "a)" 2
  expectError "trailing escape" "a\\" 2

expectParse :: String -> String -> Regex -> IO ()
expectParse label input expected =
  case parseRegex input of
    Right actual
      | actual == expected -> pure ()
      | otherwise -> failTest label ("expected " ++ show expected ++ ", got " ++ show actual)
    Left err -> failTest label ("expected success, got " ++ show err)

expectPretty :: String -> Regex -> String -> IO ()
expectPretty label regex expected =
  let actual = prettyRegex regex
   in if actual == expected
        then pure ()
        else failTest label ("expected " ++ show expected ++ ", got " ++ show actual)

expectError :: String -> String -> Int -> IO ()
expectError label input expectedColumn =
  case parseRegex input of
    Left (ParseError _ actualColumn _)
      | actualColumn == expectedColumn -> pure ()
      | otherwise -> failTest label ("expected error at column " ++ show expectedColumn ++ ", got column " ++ show actualColumn)
    Right regex -> failTest label ("expected parse error, got " ++ show regex)

failTest :: String -> String -> IO ()
failTest label message =
  error (label ++ ": " ++ message)