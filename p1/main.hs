import Control.Monad
import Data.List
import Data.Ord

-- from https://stackoverflow.com/questions/12288318/read-a-file-line-by-line
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

splitElves :: [String] -> [[String]]
splitElves s = case dropWhile (== "") s of
  [] -> []
  s' -> w : splitElves s''
    where
      (w, s'') = break (== "") s'

main = do
  lines <- readLines "input.txt"
  let inp = (map . map) read (splitElves lines) :: [[Int]]
  let sums = map sum inp :: [Int]
  let sorted = sortOn negate sums
  let out1 = maximum sums :: Int
  let out2 = sum $ take 3 sorted
  print out1
  print out2