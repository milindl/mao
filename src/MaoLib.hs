{-# LANGUAGE DeriveDataTypeable #-}

module MaoLib
where

import Data.Map.Strict as Map
import Data.Dynamic
import Control.Monad.State.Lazy
import Data.Maybe
import System.Random.Shuffle
import System.Random


data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq, Read, Enum)

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
  deriving (Show, Read, Eq, Ord, Enum)

data Player = P1
            | P2
            | P3
            | P4
  deriving (Show, Read, Eq, Enum)

nextPlayer :: Player -> Player
nextPlayer P4 = P1
nextPlayer p = succ p

prevPlayer :: Player -> Player
prevPlayer P1 = P4
prevPlayer p = pred p

data Card  = Card Suit Rank
  deriving (Show, Read, Eq)

instance Enum Card where
  toEnum i = Card (toEnum (i `div` 13)) (toEnum (i `mod` 13))
  fromEnum (Card s r) = (fromEnum s)*13 + (fromEnum r)

suit :: Card -> Suit
suit (Card s _) = s

rank :: Card -> Rank
rank (Card _ r) = r

type Deck = [Card]
type Token = Int

data GameState = GameState { decks :: Map.Map String Deck
                           , tokens :: Map.Map String Token
                           , players :: [Player]
                           , playQ :: [Player]
                           , nextStatePlayQ :: [Player]
                           , gmap :: Map.Map String Dynamic
                           , winner :: Maybe Player
                           , turnOver :: Bool
                           , stageOver :: Bool
                           , currentPlayer :: Player
                           , viewable :: Map.Map String (GameState -> String)
                           }

instance Show GameState where
  show g = "decks" ++ show (decks g) ++ "\n" ++
           "tokens" ++ show (tokens g) ++ "\n" ++
           "players" ++ show (players g) ++ "\n" ++
           "playQ" ++ show (playQ g) ++ "\n" ++
           "nextStatePlayQ" ++ show (nextStatePlayQ g) ++ "\n" ++
           "gmap" ++ show (gmap g) ++ "\n" ++
           "winner" ++ show (winner g) ++ "\n" ++
           "turnOver" ++ show (turnOver g) ++ "\n" ++
           "stageOver" ++ show (stageOver g) ++ "\n" ++
           "currentPlayer" ++ show (currentPlayer g) ++ "\n"

newtype Rule = Rule { runRule :: StateT GameState IO () }
type Stage = [Rule]
type Description = String
type StageName = String
type Turn = [(StageName, Description, Stage)]

-- Overall functions

playTurn :: Turn -> StateT GameState IO ()
playTurn turn = do
  gameState <- get
  if (winner gameState /= Nothing) then return () else do
    let announceRunner (sn, d, s) = do
          liftIO $ dashedLine
          outS $ "Starting stage: " ++ sn
          outS d
          liftIO $ dashedLine
          playStage s

    mapM_ announceRunner turn
    playTurn turn

playStage :: Stage -> StateT GameState IO ()
playStage stage = do
  gameState <- get
  -- First, make checks to see if game is already done.
  -- In that case, do not run any stage at all.
  -- a) if winner is declared
  -- b) if next turn for all players seems to be skipped
  let playersQueue = nextStatePlayQ gameState
  let newPlayersQueue = [P1, P2, P3, P4]
  let gameState' = gameState { nextStatePlayQ = newPlayersQueue }
  put gameState'
  if (winner gameState /= Nothing || length playersQueue == 0) then return () else do
  -- Now, actually run all the rules for each of the players
    mapM_ (withPlayer stage) playersQueue
    return ()
    -- gameState'' <- get
    -- out gameState'' -- for debugging only..


withPlayer :: [Rule] -> Player -> StateT GameState IO ()
withPlayer rs p = do
  gameState <- get
  let gameState' = gameState { currentPlayer = p }
  put gameState'

  if (winner gameState' /= Nothing) then return () else do
    -- Helpful messages
    outS $ "Hello, " ++ (show p)
    let choices =  [
          (0, "Continue with turn.", mapM_ playRule rs)
          ] ++
          zip3 ([1..])
          (Prelude.map (("View " ++) . fst) (toList $ viewable gameState))
          (Prelude.map  ((>> withPlayer rs p) . outS . flip snd gameState')
           (toList $ viewable gameState))

    choice choices


playRule :: Rule -> StateT GameState IO ()
playRule r = do
  gameState <- get
  if (winner gameState /= Nothing) then return () else do
    runRule r

-- temporary starting data
gStart = GameState { decks = Map.singleton "main" [Card Hearts Ace, Card Spades Ace]
                   , tokens = Map.empty
                   , players = [P1, P2, P3, P4]
                   , playQ = []
                   , nextStatePlayQ = [ P1, P2, P3, P4 ]
                   , gmap = Map.empty
                   , winner = Nothing
                   , turnOver = False
                   , stageOver = False
                   , currentPlayer = P1
                   , viewable = Map.fromList [("curr", (\g -> (show $ currentPlayer g))),
                                              ("winner", (\g -> (show $ winner g)))]
                   }

ioRule = Rule { runRule = do
                  x <- inp (t :: Int)
                  out (2 * x)
                  gameState <- get
                  when (x == 4) $ declareWinner (currentPlayer gameState)
                  return ()
              }

-- Make utility functions for DSLs!
-- bots/type inference helpers/syntactic sugar

t = error "Tried to use bot value for something but type coercion"
from = "from"
to = "to"
for = "for"
by = "by"

-- IO

choice :: [(Int, String, StateT GameState IO ())] -> StateT GameState IO ()
choice choices = do
  outS "You can do the following:"
  outS $ unwords $ Prelude.map (\(a, b, _) -> "(" ++ (show a) ++ ") " ++ b) choices
  choiceNum <- inp (t :: Int)
  let (_, _, choiceFunc) = head $ Prelude.filter ((choiceNum ==) . (\(a, _, _) -> a)) choices
  choiceFunc

out x = liftIO $ putStrLn $ show x
outS x = liftIO $ putStrLn x

inp :: (Read b) => b -> StateT GameState IO b
inp az = do
  k <- liftIO $ getLine
  let k' = read k
  return k'


-- Deck

pop :: from_ -> String -> StateT GameState IO Card
pop _ key = do
  gameState <- get
  let ds = decks gameState
      d = fromJust $ Map.lookup key ds
      top = head d
      ds' = insert key (tail d) ds
      gameState' = gameState { decks = ds' }
  put gameState'
  return top

push :: Card -> to_ -> String -> StateT GameState IO ()
push card _ key = do
  gameState <- get
  let ds = decks gameState
      d = fromJust $ Map.lookup key ds
      d' = card:d
      ds' = insert key (d') ds
      gameState' = gameState { decks = ds' }
  put gameState'
  return ()

top :: f -> String -> StateT GameState IO Card
top _ key = do
  gameState <- get
  let ds = decks gameState
      d = fromJust $ Map.lookup key ds
      top = head d
  return top

clear :: _from -> String -> StateT GameState IO ()
clear _ key = do
  gameState <- get
  let ds = decks gameState
      ds' = Map.insert key [] ds
      gameState' = gameState { decks = ds' }
  put gameState'
  return ()

createDeck :: String -> StateT GameState IO ()
createDeck key = do
  gameState <- get
  let ds = decks gameState
  let deck = Map.lookup key ds
  let gameState' = case deck of
        (Just _) -> gameState
        Nothing -> let ds' = Map.insert key [] ds
                  in gameState { decks = ds' }
  put gameState'
  return ()

getDeck :: String -> StateT GameState IO Deck
getDeck key = do
  gameState <- get
  let ds = decks gameState
  let deck = Map.lookup key ds
  case deck of
        (Just d) -> return d
        Nothing -> return []

setDeck :: String -> _to ->  Deck -> StateT GameState IO ()
setDeck key _ deck = do
  gameState <- get
  let ds = decks gameState
  let ds' = Map.insert key deck ds
  put $ gameState { decks = ds' }

shuffleDeck :: String -> StateT GameState IO ()
shuffleDeck key = do
  gameState <- get
  gen <- liftIO $ newStdGen
  let ds = decks gameState
  let d = fromJust $ Map.lookup key ds
  let shufDeck = shuffle' d (length d) gen
  let ds' = Map.insert key shufDeck ds
  put $ gameState { decks = ds' }
  return ()

-- Tokens

getTokens :: for_ -> String -> StateT GameState IO Token
getTokens _ key = do
  gameState <- get
  let ts = tokens gameState
      t = fromJust $ Map.lookup key ts
  return t

setTokens :: to_ -> Token -> for_ -> String -> StateT GameState IO ()
setTokens _ token _ key = do
  gameState <- get
  let ts = tokens gameState
      ts' = Map.insert key token ts
      gameState' = gameState { tokens = ts' }
  put gameState'
  return ()

incrementTokens :: by_ -> Token -> for_ -> String -> StateT GameState IO ()
incrementTokens _ token _ key = do
  tOld <- getTokens for key
  setTokens to (tOld + token) for key

decrementTokens :: by_ -> Token -> for_ -> String -> StateT GameState IO ()
decrementTokens _ token _ key = do
  tOld <- getTokens for key
  setTokens to (tOld - token) for key

-- Generic map

setData :: (Typeable b) => String -> to_ -> b -> b -> StateT GameState IO ()
setData key _ value _ = do
  gs <- get
  let mp = gmap gs
      mp' = Map.insert key (toDyn value) mp
      gs' = gs { gmap = mp' }
  put gs'
  return ()


getData :: (Typeable b) => String -> b -> StateT GameState IO b
getData key t_ = do
  gs <- get
  let v = fromJust $ Map.lookup key (gmap gs)
  let v' = fromDyn v t_
  return v'

isData :: String -> StateT GameState IO Bool
isData key = do
  gameState <- get
  case (Map.lookup key (gmap gameState)) of
    Nothing -> return False
    Just _ -> return True


-- Player

current :: StateT GameState IO Player
current = do
  gameState <- get
  return (currentPlayer gameState)

forPlayer :: String -> Player -> String
suffix `forPlayer` p = (show p) ++ suffix

-- Turn and stage

endTurn :: StateT GameState IO ()
endTurn = do
  endStage
  gameState <- get
  put gameState { turnOver = True }

endStage :: StateT GameState IO ()
endStage = do
  gameState <- get
  put gameState { stageOver = True }

skipNextStage :: for_ -> Player -> StateT GameState IO ()
skipNextStage _ p = do
  gameState <- get
  let pq = nextStatePlayQ gameState
  let pq' = Prelude.filter (/=p) pq
  put $ gameState { nextStatePlayQ = pq' }
  return ()

declareWinner :: Player -> StateT GameState IO ()
declareWinner p = do
  endTurn
  gameState <- get
  let gameState' = gameState { winner = Just p }
  put gameState'
  return ()

isUnaryRule :: String -> StateT GameState IO () -> StateT GameState IO ()
key `isUnaryRule` st = do
  gameState <- get
  alreadyDone <- isData key
  setData key to True (t :: Bool)
  if alreadyDone
    then return ()
    else st

-- Viewables

addViewable :: String -> _for -> (GameState -> String) -> StateT GameState IO ()
addViewable name _ fn = do
  gameState <- get
  let v = viewable gameState
  let v' = Map.insert name fn v
  let gameState' = gameState { viewable = v' }
  put gameState'
  return ()


-- aesthetics

markerLine :: IO ()
markerLine = putStrLn ""

dashedLine :: IO ()
dashedLine = putStrLn "======================================="
