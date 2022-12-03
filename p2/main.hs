import System.Environment

-- from https://stackoverflow.com/questions/12288318/read-a-file-line-by-line
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

data Move = Rock | Paper | Scissors deriving (Eq, Show, Read)

data Outcome = Win | Tie | Loss deriving (Eq, Show, Read)

shapeScore :: Move -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

play :: (Move, Move) -> Outcome
play (a, b)
  | a == b = Tie
  | a == Rock && b == Paper = Loss
  | a == Rock && b == Scissors = Win
  | a == Paper && b == Rock = Win
  | a == Paper && b == Scissors = Loss
  | a == Scissors && b == Rock = Loss
  | a == Scissors && b == Paper = Win

outcomeScore :: Outcome -> Int
outcomeScore Win = 6
outcomeScore Tie = 3
outcomeScore Loss = 0

parseChar :: Char -> Move
parseChar 'A' = Rock
parseChar 'B' = Paper
parseChar 'C' = Scissors
parseChar 'X' = Rock
parseChar 'Y' = Paper
parseChar 'Z' = Scissors

parseChar2 :: Char -> Outcome
parseChar2 'X' = Loss
parseChar2 'Y' = Tie
parseChar2 'Z' = Win

rightMove :: Move -> Outcome -> Move
rightMove m o
  | o == Tie = m
  | m == Rock && o == Win = Paper
  | m == Rock && o == Loss = Scissors
  | m == Paper && o == Win = Scissors
  | m == Paper && o == Loss = Rock
  | m == Scissors && o == Win = Rock
  | m == Scissors && o == Loss = Paper

parseLine :: String -> (Move, Move)
parseLine (a : ' ' : b : _) = (parseChar a, parseChar b)

parseLine2 :: String -> (Move, Outcome)
parseLine2 (a : ' ' : b : _) = (parseChar a, parseChar2 b)

score :: (Move, Move) -> Int
score (a, b) = outcomeScore (play (b, a)) + shapeScore b

score2 :: (Move, Outcome) -> Int
score2 (m, o) =
  let my_move = rightMove m o :: Move
   in score (m, my_move)

main = do
  args <- getArgs
  lines <- readLines (head args)
  let pairs = map parseLine lines :: [(Move, Move)]
  let scores = map score pairs
  let output1 = sum $ map score pairs
  let output2 = sum $ map (score2 . parseLine2) lines :: Int
  -- mapM_ print scores
  print output1
  print output2