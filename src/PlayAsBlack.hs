https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module PlayAsBlack where

-- import Move
import State

makeMove :: State -> Lookahead -> Moves
makeMove _ _ = concat $ replicate 4 [(x, y) | x <- [0 .. 24], y <- [1 .. 6]]
