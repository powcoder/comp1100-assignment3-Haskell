https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module PlayAsWhite where

import State
import Data.Tree

-- import Move
-- import Data.List
-- import Debug.Trace (trace)

--data GeneralTree a =
--    GeneralNode a
--                [GeneralTree a]
--    deriving (Show, Eq)

--generalTreeLeaves :: GeneralTree State -> [State]
--generalTreeLeaves (GeneralNode v []) = [v]
--generalTreeLeaves (GeneralNode _ l) = concat (map generalTreeLeaves l)

--diceState ::  Int -> Int -> [[Int]]
--diceState x y = helper1 x y
--    where
--        helper1 a b = if a==b then [[a,a,a,a] | a <- [1..6] , b <- [1..6]] else [[a,b] | a <- [1..6] , b <- [1..6]]

--oneLayerTree :: State -> [State]
--oneLayerTree (State sta boa t [x,y] wp bp ws bs) = map (helper2 (State sta boa t [x,y] wp bp ws bs) ) ( diceState x y )
--        where
--           helper2 (State sta2 boa2 t2 _ wp2 bp2 ws2 bs2) ls = State sta2 boa2 t2 ls wp2 bp2 ws2 bs2

makeMove :: State -> Lookahead -> Moves
makeMove _ l
  | primesUnder (l * l) < 0 = []
  | otherwise =
    concat $ replicate 4 $ reverse [(x, y) | x <- [0 .. 24], y <- [1 .. 6]]

primesUnder :: Int -> Int
primesUnder n = length $ filterPrime [2 .. n]
  where
    filterPrime [] = []
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
