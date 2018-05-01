# Card Language Reference

## IO

- `choice choices`, where choices is `[(Int, String, Rule)]`
  This will give is something like
  ```
  (0) Continue with turn. (1) View myscore
  ```

- `out p` where `p` has `show`
- `outS p`, where `p` is a `String`
- `c <- inp (t :: Type)`, where `Type` has `read`

## Deck

- `c <- pop from "deckName"`
- `push (Card Hearts Ace) to "deckName"`
- `c <- top from "deckName"`
- `clear from "deckName"`
- `createDeck "deckName"`
- `c <- getDeck "deckName"`
- `setDeck "deckName" deck`
- `shuffleDeck "deckName"`

## Tokens

- `c <- getTokens for "tokenName"`
- `setTokens to (5 :: Token) for "tokenName"`
- `incrementTokens by (5 :: Token) for "tokenName"`
- `decrementTokens by (5 :: Token) for "tokenName"`

## Generic Data
- `setData "key" to Value (t :: Type)`
- `c <- getData key (t :: Type)`
- `bool <- isData key`

## Player

- `p <- current`

## Turn and Stage

- `endTurn`
- `endStage`
- `declareWinner P1`
- ```"ruleId" `isUnaryRule` do { ... }```
- `skipNextStage for P1`

## Add Viewables

- `addViewable "viewableName" for (\g -> string)`
