import qualified Data.Map as M
import Control.Monad (replicateM_)

import WordHero.Trie
import WordHero.Solver
import Data.List

import System.Process (system)
import Control.Concurrent (threadDelay)

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

sayWords :: String -> IO ()
sayWords s = system mPlayer >> return () -- (threadDelay 10000)
  where
    url = "\"http://translate.google.com/translate_tts?ie=UTF-8&tl=en&q=" ++ s' ++ "\""
    mPlayer = unwords ["mplayer", "-quiet", url, "2>&1", ">", "/dev/null"]
    s' = intercalate "+" $ words s
