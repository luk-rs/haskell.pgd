import Data.List hiding (find)
import Data.Map (Map)
import Data.Map qualified as Map hiding (Map)
import System.Directory (listDirectory)

find :: String -> IO (Map Int String)
find st = do
  entries <- listDirectory "."
  let found = sort $ filter (st `isInfixOf`) entries
  let foundMap = Map.fromList $ zip ([1 ..] :: [Int]) found
  return foundMap

main :: IO ()
main = do
  putStrLn "Insert search term: "
  st <- getLine
  found <- find st
  putStrLn $ "The following entries match your search" ++ show found
