module WordHero.Trie where

import qualified Data.Map as M
import Prelude hiding (lookup)
import Data.Char (toLower)


data Trie a = Trie { value :: Maybe a
                   , children :: M.Map Char (Trie a)
                   }
  deriving Show

insert :: String -> a -> Trie a -> Trie a
insert [] v t = t { value = Just v }
insert (x':xs) v t = t { children = M.insert x (insert xs v insertAt) (children t) }
  where
    insertAt = maybe empty id $ M.lookup x (children t)
    x = toLower x'

lookup :: String -> Trie a -> Maybe a
lookup [] t = value t
lookup (x:xs) t = M.lookup x (children t) >>= (lookup xs)

empty :: Trie a
empty = Trie { value = Nothing
             , children = M.empty
             }


fromList :: [(String, a)] -> Trie a
fromList xs = foldr (uncurry insert) empty xs

isPossiblePrefix :: String -> Trie a -> Bool
isPossiblePrefix s t = not . null $ possibleCompletions s t

possibleCompletions :: String -> Trie a -> [a]
possibleCompletions [] t = allValues t
possibleCompletions (x':xs) dict = case M.lookup x (children dict) of
                                    Just t -> possibleCompletions xs t
                                    Nothing -> []
  where
    x = toLower x'

allValues :: Trie a -> [a]
allValues t = case value t of
                 Just v -> v : (concatMap allValues $ values . children $ t)
                 Nothing -> (concatMap allValues $ values . children $ t)
   where
     values = map snd . M.toList
