module Lessons.Lesson13.Forest.Levels where

import Lessons.Lesson13.Forest.Forest (Forest (..))

level1Forest :: (Num a) => Forest a
level1Forest =
  Trail
    3
    ( Trail
        7
        (Trail 3 Exit Exit Exit)
        (Trail 4 Exit Exit Exit)
        (Trail 5 Exit Exit Exit)
    )
    ( Trail
        3
        (Trail 3 Exit Exit Exit)
        (Trail 9 Exit Exit Exit)
        (Trail 5 Exit Exit Exit)
    )
    ( Trail
        5
        (Trail 3 Exit Exit Exit)
        (Trail 4 Exit Exit Exit)
        (Trail 1 Exit Exit Exit)
    )