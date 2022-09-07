{-# LANGUAGE NamedFieldPuns #-}
module FreeCell where

import Util
import Deck
import Data.Maybe
import Data.List.Split
import qualified Data.Sequence as S
import Control.Monad
import Control.Monad.State.Strict
import System.Random
import System.Random.Shuffle

canBuildOnCard :: Card -> Maybe Card -> Bool
canBuildOnCard _ Nothing = True
canBuildOnCard card1 (Just card2) = color card1 /= color card2 && rank card1 == rank card2 - 1

canBuildOn :: Card -> Column -> Bool
canBuildOn x = canBuildOnCard x . listToMaybe

altSeq :: Column -> Int
altSeq [] = 0
altSeq (card : l) = if card `canBuildOn` l then altSeq l + 1 else 1

-- how many cards need to be moved from column to build on card, if possible?
moveCards :: Column -> Card -> Maybe Int
moveCards [] _ = Nothing
moveCards column@(card0:_) card =
    if d > 0 && d <= n && even d == (color card == color card0) then Just d
    else Nothing
    where
        n = altSeq column
        d = rank card - rank card0

data Layout = Layout
    { columns :: S.Seq Column       -- always have length numOfColumns
    , cells :: S.Seq (Maybe Card)   -- always have length numOfCells
    , foundations :: S.Seq Int      -- always have length 4
    }
    deriving (Show, Read)

initialNumOfCards :: [Int]
initialNumOfCards = [7, 7, 7, 7, 6, 6, 6, 6]
numOfColumns :: Int
numOfColumns = length initialNumOfCards
numOfCells :: Int
numOfCells = 4
deck :: [Card]
deck = [Card suit rank | suit <- [minBound..maxBound], rank <- [1..13]]

makeLayout :: S.Seq Column -> Layout
makeLayout columns = Layout
    { columns
    , foundations = S.replicate 4 0
    , cells = S.replicate numOfCells Nothing
    }

deal :: RandomGen g => g -> S.Seq Column
deal g = S.fromList . splitPlaces initialNumOfCards $ shuffle' deck (length deck) g

win :: Layout -> Bool
win Layout {foundations} = all (==13) foundations

{- there are five cases for moving cards around:
 - cell -> foundation
 - cell -> tableau
 - tableau -> foundation
 - tableau -> cell
 - tableau -> tableau
 - (cell -> cell is pointless and foudation -> * is forbidden)
 - among these cases, tableau -> tableau requires additional computation,
 - and the rest can be treated uniformly.
 -}

maxMoveNum :: Layout -> Int
maxMoveNum Layout {columns, cells} =
    let m = length . S.filter null $ cells
        n = length . S.filter null $ columns
    in  (m + 1) * 2^n

data CardFrom = Column Int | Cell Int
data CardTo = Depot Int | AnyCell | Foundation
data Error = NoCard | Unacceptable | Vacancy | TooLong Int

move' :: CardFrom -> CardTo -> (Card -> Bool) -> Layout -> Either Error Layout
move' from to check layout = do
    card <- take from layout <?> NoCard
    guard (check card) <?> Unacceptable
    remove from <$> (accept card to layout <?> Unacceptable)
    where
    take :: CardFrom -> Layout -> Maybe Card
    take (Column i) = S.lookup i . columns >=> listToMaybe
    take (Cell i) = (`S.index` i) . cells

    remove :: CardFrom -> Layout -> Layout
    remove (Column i) layout@Layout {columns} = layout { columns = S.adjust' tail i columns }
    remove (Cell i) layout@Layout {cells} = layout { cells = S.update i Nothing cells }

    accept :: Card -> CardTo -> Layout -> Maybe Layout
    accept card (Depot i) layout@Layout {columns} =
        card `canBuildOn` (columns `S.index` i) ~> layout { columns = S.adjust' (card:) i columns }
    accept card AnyCell layout@Layout {cells} = do
        i <- S.findIndexL null cells
        return layout { cells = S.update i (Just card) cells }
    accept Card {suit, rank} Foundation layout@Layout {foundations} =
        let i = fromEnum suit in
        rank == (foundations `S.index` i) + 1 ~> layout { foundations = S.adjust' (+1) i foundations }


move :: CardFrom -> CardTo -> Layout -> Either Error Layout
move (Column i) (Depot j) layout@Layout {columns} = do
    let source = columns `S.index` i
        dest = columns `S.index` j
    card <- listToMaybe dest <?> Vacancy
    k <- moveCards source card <?> Unacceptable
    guard (k <= maxMoveNum layout) <?> TooLong k
    let (a, b) = splitAt k source
    return layout { columns = S.adjust (a++) j . S.update i b $ columns }
move from to layout = move' from to (const True) layout

-- this is a separate function since it requires an additional parameter
-- (number of cards to move)
moveToVacancy :: Int -> Int -> Int -> Layout -> Either Error Layout
moveToVacancy i j n layout@Layout {columns} = do
    let source = columns `S.index` i
        dest = columns `S.index` j
        maxMove = maxMoveNum layout
    guard (null dest && 0 < n && n <= altSeq source) <?> Unacceptable
    guard (n <= maxMove) <?> TooLong n
    let (a, b) = splitAt n source
    return layout { columns = S.adjust (a++) j $ S.update i b columns }


autoCollect :: Layout -> Layout
autoCollect = execState (doWhileM_ collectAll)
    where
    collectAll :: State Layout Bool
    collectAll = do
        foundations <- gets foundations
        let r = min (foundations `S.index` fromEnum Spades) (foundations `S.index` fromEnum Clubs) + 2
            b = min (foundations `S.index` fromEnum Hearts) (foundations `S.index` fromEnum Diamonds) + 2
            free card = (color card == Red && rank card <= r) || (color card == Black && rank card <= b)
            collect from = stateful (move' from Foundation free)
        f1 <- sequence [collect (Cell i) | i <- [0 .. numOfCells-1]]
        f2 <- sequence [collect (Column i) | i <- [0 .. numOfColumns-1]]
        return $ or f1 || or f2
        where
        modifyWith :: (s -> Maybe s) -> s -> (Bool, s)
        modifyWith f x = case f x of
            Just y -> (True, y)
            Nothing -> (False, x)

        stateful :: (s -> Either a s) -> State s Bool
        stateful f = state . modifyWith $ getRight . f
