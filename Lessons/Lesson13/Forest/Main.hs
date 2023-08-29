module Lessons.Lesson13.Forest.Main where

import GHC.Base (IO (IO))
import Lessons.Lesson13.Forest.Forest (Forest (Exit))
import Lessons.Lesson13.Forest.Gameplay (AvailableMoves, move)
import Lessons.Lesson13.Forest.Levels (level1Forest)

main :: IO ()
main = do
  putStrLn "You're trapped in a forest and you have to try to escape. You will loose stamina for each step that you make. Choose wisely (',')"
  playGame (10, level1Forest)
  where
    playGame (_, Exit) = putStrLn "WHOOHOO, you reached the exit, congratulations!!!!! \\o/"
    playGame (stamina, _) | stamina <= 0 = putStrLn "Bummer... you ran out of stamina, you're a sitting duck in the forest -.-' "
    playGame (stamina, forest) = do
      putStrLn $ "You have " ++ show stamina ++ " stamina left. Choose your next steps carefully"
      step <- getLine
      playGame $ move (stamina, forest) (read step :: AvailableMoves)