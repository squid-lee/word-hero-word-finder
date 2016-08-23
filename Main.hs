import qualified Data.Map as M
import Control.Monad (replicateM_)

import WordHero.Trie
import WordHero.Solver
import Data.List

main = do
  dict <- crackLibDict
  putStrLn "Please type in board as one row, spaces between tiles"
  board' <- getLine
  let board = words board'
  let solutions = allWords board dict
  mapM_ print $ allWords board dict


dictAsTrie :: String -> IO (Trie String)
dictAsTrie filePath = do
  dict <- readFile filePath
  let splitByLine = filter (notElem '\'') $ lines dict
  return . fromList $ zip splitByLine splitByLine

crackLibDict = dictAsTrie "/home/joseph/dict"
