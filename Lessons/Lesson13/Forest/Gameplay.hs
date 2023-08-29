module Lessons.Lesson13.Forest.Gameplay where

import Lessons.Lesson13.Forest.Forest (Forest (..))

data AvailableMoves = GoLeft | GoForward | GoRight deriving (Show, Read)

move :: (Num a) => (a, Forest a) -> AvailableMoves -> (a, Forest a)
move (stamina, Exit) _ = (stamina, Exit)
move (stamina, Trail effort l _ _) GoLeft = (stamina - effort, l)
move (stamina, Trail effort _ f _) GoForward = (stamina - effort, f)
move (stamina, Trail effort _ _ r) GoRight = (stamina - effort, r)