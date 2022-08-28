module Deck where

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Enum, Bounded)
type Rank = Int
data Color =
      Red   -- the color of desire
    | Black -- the color of despair
    deriving (Eq)
data Card = Card
    { suit :: Suit
    , rank :: Rank
    }

instance Show Card where
    show card = "♠♥♦♣" !! fromEnum (suit card) : ["A23456789TJQK" !! (rank card - 1)]
 
colorOf :: Suit -> Color
colorOf Spades = Black
colorOf Hearts = Red
colorOf Diamonds = Red
colorOf Clubs = Black

color :: Card -> Color
color = colorOf . suit

maybeCard :: Int -> Rank -> Maybe Card
maybeCard _ 0 = Nothing
maybeCard suit rank = Just $ Card (toEnum suit) rank

type Column = [Card]
