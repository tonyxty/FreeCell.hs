module Deck where

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Enum, Bounded, Show, Read)
type Rank = Int
data Color =
      Red   -- the color of desire
    | Black -- the color of despair
    deriving (Eq)
data Card = Card
    { suit :: Suit
    , rank :: Rank
    }
    deriving (Show, Read)
 
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
