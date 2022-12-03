https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module Player where

import Test.QuickCheck (Arbitrary, arbitrary, oneof)

data Player
  = White
  | Black
  deriving (Show, Eq)

instance Arbitrary Player where
  arbitrary = oneof $ fmap return [Black, White]

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White
