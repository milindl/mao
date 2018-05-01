module Examples where

import MaoLib
import AddRuleLib
import Data.Map as Map
import Data.Maybe
import Control.Monad.State.Lazy

-- The first, very basic game
scoreViewer = \g -> let cp = currentPlayer g
                   in fromJust $ Map.lookup ("Score" `forPlayer` cp) (tokens g)

basicStart = GameState {  decks = Map.fromList [("main", [Card Spades Two .. Card Clubs Ace])]
                       ,  tokens = Map.fromList [("Score" `forPlayer` p, 0) | p <- [P1 .. P4]]
                       , playQ = []
                       , players = [P1 .. P4]
                       , nextStatePlayQ = [P1 .. P4]
                       , gmap = Map.empty
                       , winner = Nothing
                       , turnOver = False
                       , stageOver = False
                       , currentPlayer = P1
                       , viewable = Map.fromList [("myscore", show.scoreViewer)]
                       }

basicRule1 = Rule { runRule = do
                      "dist" `isUnaryRule` do
                        shuffleDeck "main"
                        d <- getDeck "main"
                        createDeck "p1Deck"
                        setDeck ("Deck" `forPlayer` P1)  to (Prelude.take 13 d)
                        setDeck ("Deck" `forPlayer` P2)  to (Prelude.take 13 (Prelude.drop 13 d))
                        setDeck ("Deck" `forPlayer` P3)  to (Prelude.take 13 (Prelude.drop 26 d))
                        setDeck ("Deck" `forPlayer` P4)  to (Prelude.take 13 (Prelude.drop 39 d))
                      }

basicRule2 = Rule  {runRule = do
                       p <- current
                       deck <- getDeck ("Deck" `forPlayer` p)
                       outS "Please select a card to play:"
                       outS (concat $ Prelude.concatMap ((: ["\n"]) . show) deck)
                       c <- inp (t :: Card)
                       if or (Prelude.map (==c) deck)
                         then do
                           incrementTokens by (fromEnum c) for ("Score" `forPlayer` p)
                           let deck' = Prelude.filter (/=c) deck
                           setDeck ("Deck" `forPlayer` p) to deck'
                         else do
                           outS "That card is not there in your deck. Your turn has been skipped."
                           return ()
                       }

basicRule3 = Rule { runRule = do
                      score1 <- getTokens for "P1Score"
                      score2 <- getTokens for "P2Score"
                      score3 <- getTokens for "P3Score"
                      score4 <- getTokens for "P4Score"
                      let maxScore = maximum [score1, score2, score3, score4]
                      if maxScore < 52
                        then return ()
                        else let det ms | (ms == score1) = declareWinner P1
                                        | (ms == score2) = declareWinner P2
                                        | (ms == score3) = declareWinner P3
                                        | otherwise = declareWinner P4
                                 in det maxScore
                  }

basicGMap = GameMap { rules = [["setup"], ["play", "winCheck"], []],
                      turn = [("Setup", "", [basicRule1])
                             , ("Play Stage", "", [basicRule2, basicRule3])
                             , ("Score View Stage", "", [])]
                    }


-- Sample of basic rule that can be added to skip even turns
-- p <- current;
-- score <- getTokens for ("Score" `forPlayer` p);
-- if (score `mod` 2 == 0) then skipNextStage for p else return();
-- incrementTokens by (1 :: Token) for ("Score" `forPlayer` p)
-- END

