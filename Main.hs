import qualified Data.Map as M
import Control.Monad (replicateM_)
import Data.List (sortBy, group)
import Data.Function (on)

import WordHero.Trie
import WordHero.Solver

main = do
  dict <- dictAsTrie
  putStrLn "Please type in board as one row, spaces between tiles"
  board' <- getLine
  let board = words board'
  let solutions = allWords board dict
  mapM_ print $ reverse $ map head $ group $ sortBy (compare `on` wordScore) $ allWords board dict


dictAsTrie :: IO (Trie String)
dictAsTrie = do
  dict <- readFile "/home/joseph/dict"
  let splitByLine = filter (notElem '\'') $ lines dict
  return . fromList $ zip splitByLine splitByLine
