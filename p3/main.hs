import Data.Char
import Data.Set (Set)
import Data.Set qualified as Set
import System.Environment

-- from https://stackoverflow.com/questions/12288318/read-a-file-line-by-line
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

strLen :: String -> Int
strLen "" = 0
strLen (s : x) = 1 + strLen x

splitTwo :: String -> [String]
splitTwo s =
  let n = strLen s
      half_n = n `div` 2
      (a, b) = splitAt half_n s
   in [a, b]

findCommonG :: [String] -> Char
findCommonG s = head $ Set.elems $ foldr1 Set.intersection $ map Set.fromList s

scoreChar :: Char -> Int
scoreChar c = if isLower c then ord c - ord 'a' + 1 else ord c - ord 'A' + 27

groupInto3 :: [String] -> [[String]]
groupInto3 [] = []
groupInto3 s = take 3 s : groupInto3 (drop 3 s)

main = do
  args <- getArgs
  lines <- readLines (head args)
  let commons = map (findCommonG . splitTwo) lines :: [Char]
  let out1 = sum $ map scoreChar commons
  let groups = groupInto3 lines :: [[String]]
  let commons2 = map findCommonG groups :: [Char]
  let out2 = sum $ map scoreChar commons2
  -- mapM_ print scores
  print out1
  print out2