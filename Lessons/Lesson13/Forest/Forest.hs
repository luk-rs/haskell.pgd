module Lessons.Lesson13.Forest.Forest where

data Forest a
  = Exit
  | Trail a (Forest a) (Forest a) (Forest a)
  deriving (Show)