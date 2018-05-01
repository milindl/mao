module Main where

import MaoLib
import AddRuleLib
import Control.Monad
import Language.Haskell.Interpreter hiding (get)
import Control.Monad.State.Lazy
import Examples
import Data.Maybe

main :: IO ()
main = do
  -- let g = GameMap { rules = [[]], turn = [("stage 1", "", [])] }
  -- g' <- endOfRound basicGMap
  playGame basicGMap
  return ()

playGame :: GameMap -> IO GameMap
playGame g = do
  let t = turn g
  let s = playTurn t
  (val, s) <- runStateT s basicStart
  let wnr = (show . fromJust . winner) s
  putStrLn $ "The winner is here: " ++ wnr
  g' <- endOfRound g
  playGame g'
