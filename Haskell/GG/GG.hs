module GeographyGame where

-- Library
import Data.List

-- gets the blowing elements in the list
pick :: [(String, Bool)] -> [String]
pick v = map fst $ filter (\(_,b) -> not b) v

-- creates adjacency list of game
digraph :: [String] -> [(String, String)]
digraph z = [ (x, y)
            | x <- z
            , y <- z
            , link x y ]
  where
    link s t = compare s t /= EQ && s /= [] && t /= [] && last s == head t

-- gets the available moves
moves :: String -> [(String, String)] -> [String]
moves s g = map snd $ filter (\(f,_) -> f == s) g

-- checks if the current state of the game is a winning state
win :: [String] -> Bool
win = null

{- Breath First Search traversal on the current game state and 
   determines if 's' leads to a blowing move
-}
bfs :: String -> [(String, String)] -> (String, Bool)
bfs s g
    | win adj   = (s, False)
    | otherwise = (s, not $ all (\i -> snd i) $ map (\s1 -> bfs s1 newState) adj)
    where
      adj      = moves s g
      newState = filter (\(f,t) -> f /= s && t /= s) g

blowingNames :: [String] -> [String]
blowingNames z = pick $ map (\s0 -> bfs s0 g) z
  where
    g = digraph z
