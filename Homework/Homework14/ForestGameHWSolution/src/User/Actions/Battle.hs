{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- {-# HLINT ignore "Redundant lambda" #-}

module User.Actions.Battle where

import qualified Control.Monad.IO.Class
import System.Random (randomRIO)

data Golem = Golem {gAttack :: Int, gHp :: Int} deriving (Show)

data Player = Player {pAttack :: Int, pHp :: Int} deriving (Show)

data Battle = Fight | RunAway deriving (Show, Read)

type BattleOutcome = ((Player, Golem), Outcome)

data Outcome = Win | Loss | Empass

battle :: IO Bool
battle = do
  player <- gen Player
  golem <- gen Golem
  putStrLn "You've encountered a golem"
  printStats player golem
  battle' player golem
  where
    gen f = uncurry f <$> generateStats

printStats :: Player -> Golem -> IO ()
printStats player golem = do
  putStrLn "YOUR STATS"
  putStrLn $ (show . pHp $ player) ++ "hp\t" ++ (show . pAttack $ player) ++ "ad"
  putStrLn "GOLEM STATS"
  putStrLn $ (show . gHp $ golem) ++ "hp\t" ++ (show . gAttack $ golem) ++ "ad"

battle' :: Player -> Golem -> IO Bool
battle' player golem = do
  putStrLn "What do you want to do next? (Fight | RunAway)"
  line <- getLine
  let option = read @Battle line
  battleOutcome <- case option of
    Fight -> fight player golem
    RunAway -> runAway player golem
  uncurry printStats (fst battleOutcome)
  let outcome = snd battleOutcome
  case outcome of
    Loss -> return False
    Win -> return True
    Empass -> uncurry battle' (fst battleOutcome)

fight :: Player -> Golem -> IO BattleOutcome
fight player golem = runEvent (player, updatePlayer) (golem, updateGolem) winCondition lossCondition
  where
    updatePlayer p g = p {pHp = pHp p - gAttack g}
    updateGolem p g = g {gHp = gHp g - pAttack p}
    winCondition _ g _ = gHp g <= 0
    lossCondition p _ _ = pHp p <= 0

runAway :: Player -> Golem -> IO BattleOutcome
runAway player golem = runEvent (player, updatePlayer) (golem, updateGolem) winCondition lossCondition
  where
    updatePlayer p g = p {pHp = pHp p - gAttack g}
    updateGolem _ g = g
    winCondition _ _ d = d
    lossCondition p _ _ = pHp p <= 0

runEvent :: Control.Monad.IO.Class.MonadIO m => (a1, a1 -> a2 -> a1) -> (a2, a1 -> a2 -> b) -> (a1 -> b -> Bool -> Bool) -> (a1 -> b -> Bool -> Bool) -> m ((a1, b), Outcome)
runEvent (player, updatePlayer) (golem, updateGolem) winCondition lossCondition = do
  rand <- randomRIO @Int (0, 1)
  let dodged = rand < 1
      outcome = (if dodged then (player, updateGolem player golem) else (updatePlayer player golem, updateGolem player golem))
      (pUpdated, gUpdated) = outcome
      lost = lossCondition pUpdated gUpdated dodged
      won = not lost && winCondition pUpdated gUpdated dodged
  case (lost, won) of
    (True, _) -> return (outcome, Loss)
    (_, True) -> return (outcome, Win)
    _ -> return (outcome, Empass)

generateStats :: IO (Int, Int)
generateStats = do
  hp <- randomRIO @Int (50, 100)
  attack <- randomRIO @Int (10, 35)
  return (attack, hp)