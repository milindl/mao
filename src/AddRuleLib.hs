module AddRuleLib where

import MaoLib
import Control.Monad
import Language.Haskell.Interpreter hiding (get)
import Control.Monad.State.Lazy
import Data.Map as Map

data GameMap = GameMap { rules :: [[String]]
                       , turn :: Turn
                       }

getRuleInput :: IO String
getRuleInput = do
  l <- getLine
  if l == "END"
    then return ""
    else do
      l2 <- getRuleInput
      return (l ++ l2)

getRule :: Interpreter Rule
getRule = do
  setImportsQ [("Data.Map", Just "Map"), ("Prelude", Nothing)]
  -- For some reason, only the absolute level path seems to work!
  loadModules ["/home/milind/Programs/git/CS653Project/mao/src/MaoLib.hs"]
  setTopLevelModules ["MaoLib"]
  rule <- liftIO $ getRuleInput
  liftIO $ putStrLn $ "Rule {runRule = do {" ++ rule ++ "} }"
  liftIO $ putStrLn "Please be patient, it may take a moment for the rule to be added."
  liftIO $ markerLine
  interpret  ("Rule {runRule = do {" ++ rule ++ "} }") (as :: Rule)

-- Set up the actual game here, and loop it!
addStage :: StageName -> Description -> GameMap -> GameMap
addStage sn d g = let t = turn g
                      r = rules g
                  in g { turn = t ++ [(sn, d, [])], rules = r ++ [[]] }

addRule :: Int -> Int -> String -> Rule -> GameMap -> GameMap
addRule stage pos name rule g = let rs = rules g
                                    rulez = rs !! stage
                                    rulez' = (Prelude.take pos rulez)
                                      ++ [name]
                                      ++ (Prelude.drop pos rulez)
                                    rs' = (Prelude.take stage rs)
                                      ++ [rulez']
                                      ++ (Prelude.drop (stage + 1) rs)
                                    ss = turn g
                                    (sn, d, s) = ss !! stage
                                    s' = (Prelude.take pos s)
                                      ++ [rule]
                                      ++ (Prelude.drop pos s)
                                    ss' = (Prelude.take stage ss)
                                      ++ [(sn, d, s')]
                                      ++ (Prelude.drop (stage + 1) ss)
                                    in g { rules = rs', turn = ss' }


-- This is the workhorse method to actually do IO and get a new rule into the game map
endOfRound :: GameMap -> IO GameMap
endOfRound g = do
  putStrLn "You can now add a rule. Please select a stage in which to add the rule:"
  let stageLst = zip [0..] (Prelude.map (\(sn, _, _) -> sn) (turn g))
      stageLst' = stageLst ++ [(length stageLst, "New stage?")]
  putStrLn $ unwords $ (Prelude.map (\(x, y) -> "(" ++ (show x) ++ ") " ++ y) stageLst')
  stageNum <- getLine
  let stageNum' = read stageNum :: Int
  g' <- if (stageNum' == length stageLst) then do
    putStrLn "StageName: "
    sn <- getLine
    markerLine
    putStrLn "Description: "
    d <- getLine
    markerLine
    return (addStage sn d g)
    else return g

  -- Now, get the rule
  putStrLn "Where in this stage would you like to enter your rule? A list of rules with their index is given for reference. (It may be empty, in which case you should enter a 0)"
  putStrLn "Rules: "
  putStrLn $ unwords $ zipWith (\x y -> "(" ++ (show x) ++ ") " ++ y) [0..] (rules g' !! stageNum')
  rulePos <- getLine
  putStrLn "Rule Name:"
  rName <- getLine
  markerLine
  let rulePos' = read rulePos :: Int
  rule <- readRuleTillRight
  return $ addRule stageNum' rulePos' rName rule g'



readRuleTillRight :: IO Rule
readRuleTillRight = do
  putStrLn "Enter rule, terminate by END"
  rule <- runInterpreter getRule
  case rule of
    (Left e) -> do
      putStrLn "There was some issue with the rule you entered"
      putStrLn (show e)
      markerLine
      readRuleTillRight
    (Right rule') ->   return rule'
