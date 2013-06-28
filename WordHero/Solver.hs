module WordHero.Solver where

import Data.List ((\\), reverse, sortBy, group)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Prelude hiding (lookup)
import qualified Data.Map as M

import WordHero.Trie

type Path = (Int, [Int])
type Board = [String]
type Dict = Trie String


adjacentValues :: Int -> [Int]
adjacentValues 0 = [1, 4, 5]
adjacentValues 1 = [0, 4, 5, 6, 2]
adjacentValues 2 = [1, 5, 6, 3, 7]
adjacentValues 3 = [2, 6, 7]
adjacentValues 4 = [0, 1, 5, 8, 9]
adjacentValues 5 = [0, 1, 2, 4, 6, 8, 9, 10]
adjacentValues 6 = [1, 2, 3, 5, 7, 9, 10, 11]
adjacentValues 7 = [2, 3, 6, 10, 11]
adjacentValues 8 = [4, 5, 9, 12, 13]
adjacentValues 9 = [4, 5, 6, 8, 10, 12, 13, 14]
adjacentValues 10 = [5, 6, 7, 9, 11, 13, 14, 15]
adjacentValues 11 = [6, 7, 10, 14, 15]
adjacentValues 12 = [8, 9, 13]
adjacentValues 13 = [8, 9, 10, 12, 14]
adjacentValues 14 = [9, 10, 11, 13, 15]
adjacentValues 15 = [10, 11, 14]

possibleSteps :: Int -> [Int] -> [Int]
possibleSteps n hist = (adjacentValues n) \\ hist

step :: Path -> [Path]
step (pos, hist) = map (\x -> (x, hist')) candidateNeighbours
  where
    hist' = pos : hist
    candidateNeighbours = possibleSteps pos hist

trimStep :: Board -> Dict -> Path -> [Path]
trimStep b d p = trimStates b d $ step p

nStep :: Int -> Board -> Dict -> Path -> [Path]
nStep n b d p = foldl (>>=) [p] (replicate n (\p -> trimStep b d p))

trimStates :: Board -> Dict -> [Path] -> [Path]
trimStates board dict states = filter (\x -> isPossiblePrefix (stateToStr x board) dict) states

stateToStr :: Path -> Board -> String
stateToStr (pos, hist) board = concatMap (board!!) $ reverse (pos : hist)

allForPos :: Int -> Board -> Dict -> [String]
allForPos n b d = onlyLegitWords
  where
    allLegitPaths = concatMap (\stepNo -> nStep stepNo b d (n, [])) [2..15]
    legitPathsAsWords = map (flip stateToStr b) allLegitPaths
    onlyLegitWords = mapMaybe (`lookup` d) legitPathsAsWords

allWords :: Board -> Dict -> [String]
allWords b d = orderDescByScoreWithoutDuplicates $
               concatMap (\n -> allForPos n b d) [0..15]
  where
    orderDescByScoreWithoutDuplicates = reverse
                                        . map head
                                        . group
                                        . sortBy (compare `on` wordScore)

wordScore :: String -> Int
wordScore [] = 0
wordScore (x:y:w) = case letterScores [x,y] of
                      Just n -> n + wordScore w
                      Nothing -> wordScore [x] + wordScore (y:w)
wordScore (l:w) = (maybe 0 id (letterScores [l])) + wordScore w


letterScores :: String -> Maybe Int
letterScores s = M.lookup s (M.fromList [ ("a",1)
                                        , ("b",3)
                                        , ("c",2)
                                        , ("d",2)
                                        , ("e",1)
                                        , ("f",4)
                                        , ("g",2)
                                        , ("h",2)
                                        , ("i",2)
                                        , ("j",9)
                                        , ("k",6)
                                        , ("l",2)
                                        , ("m",2)
                                        , ("n",2)
                                        , ("ng",4)
                                        , ("o",2)
                                        , ("on", 4)
                                        , ("p",2)
                                        , ("qu",11)
                                        , ("r",2)
                                        , ("s",2)
                                        , ("t",1)
                                        , ("u",2)
                                        , ("v",5)
                                        , ("w",6)
                                        , ("x",9)
                                        , ("y",3)])
