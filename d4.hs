import Data.List.Split
import System.Environment

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

data MRange = MRange
  { lo :: Int,
    hi :: Int
  }
  deriving (Show)

parseRange :: String -> MRange
parseRange s =
  let sr = splitOn "-" s
   in MRange {lo = read $ head sr, hi = read $ sr !! 1}

parseRanges :: String -> (MRange, MRange)
parseRanges s =
  let ss = splitOn "," s
      ranges = map parseRange ss
   in (head ranges, ranges !! 1)

overLapping0 :: (MRange, MRange) -> Bool
overLapping0 (a, b) = lo a <= lo b && hi a >= hi b

overLapping :: (MRange, MRange) -> Bool
overLapping (a, b) = overLapping0 (a, b) || overLapping0 (b, a)

overLapping2 :: (MRange, MRange) -> Bool
overLapping2 (a, b) = (lo a <= lo b && hi a >= lo b) ||
  (lo b <= lo a && hi b >= lo a)

main = do
  args <- getArgs
  lines <- readLines (head args)
  let ranges = map parseRanges lines
  let overlapping = map overLapping ranges
  let out1 = length $ filter id overlapping
  let overlapping2 = map overLapping2 ranges
  let out2 = length $ filter id overlapping2
  print out1
  print out2