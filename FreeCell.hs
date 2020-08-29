#!/usr/bin/env stack
{- stack script
 --resolver lts-16.10
 --compile
-}
{-# LANGUAGE NamedFieldPuns #-}
import Data.Char
import Data.Either.Combinators
import Data.List.Split
import Data.Maybe
import qualified Data.Sequence as S
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Text.Read hiding (get)
import System.IO
import System.Random
import System.Random.Shuffle
import qualified System.Console.ANSI as C

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

color :: Card -> Color
color card = case suit card of
    Spades -> Black
    Hearts -> Red
    Diamonds -> Red
    Clubs -> Black

maybeCard :: Int -> Rank -> Maybe Card
maybeCard _ 0 = Nothing
maybeCard suit rank = Just $ Card (toEnum suit) rank

canBuildOn :: Card -> Maybe Card -> Bool
canBuildOn _ Nothing = True
canBuildOn card1 (Just card2) =
    color card1 /= color card2 && rank card1 == rank card2 - 1

instance Show Card where
    show card = "♠♥♦♣" !! fromEnum (suit card) :
        ["A23456789TJQK" !! (rank card - 1)]
 
type Cascade = [Card]

tableau :: Cascade -> Int
tableau [] = 0
tableau (card : l) = if card `canBuildOn` (listToMaybe l)
                     then tableau l + 1 else 1

moveCards :: Cascade -> Card -> Maybe Int
moveCards [] _ = Nothing
moveCards cascade@(card0:_) card =
-- how many cards need to be moved from cascade to build on card, if possible?
    if d > 0 && d <= n && (even d) == (color card == color card0) then Just d
    else Nothing
    where
        n = tableau cascade
        d = rank card - rank card0

data Configuration = Configuration
    { cascades :: S.Seq Cascade         -- always have length numOfCascades
    , foundations :: S.Seq Int          -- always have length 4
    , cells :: S.Seq (Maybe Card)       -- always have length numOfCells
    , lastError :: Maybe String
    }

initialNumOfCards :: [Int]
initialNumOfCards = [7, 7, 7, 7, 6, 6, 6, 6]
numOfCascades :: Int
numOfCascades = length initialNumOfCards
numOfCells :: Int
numOfCells = 4
deck :: [Card]
deck = [Card suit rank | suit <- [minBound..maxBound], rank <- [1..13]]

moveFromCell :: Int -> Int -> Configuration -> Maybe Configuration
moveFromCell i j conf@(Configuration {cascades, cells}) = do
-- move the card in the i-th cell to the j-th cascade
    card <- cells `S.index` i
    guard $ card `canBuildOn` listToMaybe (cascades `S.index` j)
    return conf { cascades = S.adjust (card:) j cascades
                , cells = S.update i Nothing cells
                }

moveToCell :: Int -> Maybe Int -> Configuration -> Maybe Configuration
moveToCell i j' conf@(Configuration {cascades, cells}) = do
    card <- listToMaybe $ cascades `S.index` i
    j <- j' <|> S.findIndexL null cells
    guard $ isNothing $ cells `S.index` j
    return conf { cascades = S.adjust tail i cascades
                , cells = S.update j (Just card) cells
                }

moveToFoundation :: Int -> Configuration -> Maybe Configuration
moveToFoundation i conf@(Configuration {cascades, foundations}) = do
    card <- listToMaybe $ cascades `S.index` i
    let s = fromEnum $ suit card
    guard $ rank card - 1 == foundations `S.index` s
    return conf
        { cascades = S.adjust tail i cascades
        , foundations = S.adjust (+1) s foundations
        }

moveCellToFoundation :: Int -> Configuration -> Maybe Configuration
moveCellToFoundation i conf@(Configuration {foundations, cells}) = do
    card <- cells `S.index` i
    let s = fromEnum $ suit card
    guard $ rank card - 1 == foundations `S.index` s
    return conf { foundations = S.adjust (+1) s foundations
                , cells = S.update i Nothing cells
                }

maxMoveNum :: Configuration -> Int
maxMoveNum (Configuration {cascades, cells}) =
    let m = length $ S.filter isNothing cells
        n = length $ S.filter null cascades
    in  (m + 1) * 2^n

moveTableau :: Int -> Int -> Configuration -> Maybe Configuration
moveTableau i j conf@(Configuration {cascades}) =
    let source = cascades `S.index` i
        dest = cascades `S.index` j
    in  if null dest then moveToEmptyCascade i j 1 conf
        else do
            card <- listToMaybe dest
            k <- moveCards source card
            guard $ k <= maxMoveNum conf
            let (a, b) = splitAt k source
            return conf { cascades = S.adjust (a++) j $ S.update i b cascades }

-- this is a separate function since it requires an additional parameter
-- (number of cards to move)
moveToEmptyCascade :: Int -> Int -> Int -> Configuration -> Maybe Configuration
moveToEmptyCascade i j n conf@(Configuration {cascades}) = do
    let source = cascades `S.index` i
        dest = cascades `S.index` j
        maxMove = maxMoveNum conf
    guard $ null dest && n <= maxMove && 0 < n && n <= tableau source
    let (a, b) = splitAt n source
    return conf { cascades = S.adjust (a++) j $ S.update i b cascades }


data CardPos = Cascade Int
             | Foundation
             | Cell Int
             | AnyCell

move :: CardPos -> CardPos -> Configuration -> Maybe Configuration
move (Cell f) Foundation = moveCellToFoundation f
move (Cell f) (Cascade t) = moveFromCell f t
move (Cascade f) Foundation = moveToFoundation f
move (Cascade f) (Cell t) = moveToCell f (Just t)
move (Cascade f) AnyCell = moveToCell f Nothing
move (Cascade f) (Cascade t) = moveTableau f t
move _ _ = const Nothing

autoCollect :: Configuration -> Configuration
autoCollect conf@(Configuration {cascades, foundations, cells}) =
    let r = min (foundations `S.index` fromEnum Spades)
                (foundations `S.index` fromEnum Clubs) + 2
        b = min (foundations `S.index` fromEnum Hearts)
                (foundations `S.index` fromEnum Diamonds) + 2
        (f', conf') = runState (sequence $
            S.mapWithIndex (tryMoveFromCell r b) cells) conf
        (f'', conf'') = runState (sequence $
            S.mapWithIndex (tryMove r b) cascades) conf'
    in  if any id f' || any id f'' then autoCollect conf'' else conf''
    where
        tryMoveFromCell :: Int -> Int -> Int -> Maybe Card ->
            State Configuration Bool
        tryMoveFromCell _ _ _ Nothing = return False
        tryMoveFromCell r b i (Just card) =
            if ((color card == Red && rank card <= r) ||
                (color card == Black && rank card <= b)) then do
                conf <- get
                case moveCellToFoundation i conf of
                    Just conf' -> put conf' >> return True
                    Nothing -> return False
            else return False

        tryMove :: Int -> Int -> Int -> Cascade -> State Configuration Bool
        tryMove _ _ _ [] = return False
        tryMove r b i (card:_) =
            if ((color card == Red && rank card <= r) ||
                (color card == Black && rank card <= b)) then do
                conf <- get
                case moveToFoundation i conf of
                    Just conf' -> put conf' >> return True
                    Nothing -> return False
            else return False


deal :: RandomGen g => g -> Configuration
deal gen = Configuration 
    { cascades = S.fromList $ splitPlaces initialNumOfCards $
        shuffle' deck (length deck) gen
    , foundations = S.replicate 4 0
    , cells = S.replicate numOfCells Nothing
    , lastError = Nothing
    }

setColor :: Maybe Color -> IO ()
setColor Nothing = C.setSGR [C.SetColor C.Foreground C.Dull C.White]
setColor (Just color) =
    C.setSGR [C.SetColor C.Foreground C.Dull $
        if color == Red then C.Red else C.White]

renderCard :: Card -> IO ()
renderCard card = setColor (Just $ color card) >> putStr (show card)

renderMaybe :: Maybe Card -> IO ()
renderMaybe Nothing = setColor Nothing >> putStr "[  ]"
renderMaybe (Just card) = putChar ' ' >> renderCard card >> putChar ' '

renderCascade :: Cascade -> IO ()
renderCascade cascade = do
    -- bug (in ANSI terminal standard?):
    -- C.cursorDown 0 will cause the cursor to move down by 1 line
    let l = length cascade
    if l /= 0 then C.cursorDown l else return ()
    mapM_ (\card -> renderCard card >> C.cursorUp 1 >> C.cursorBackward 2) cascade

render :: Configuration -> IO ()
render (Configuration {cascades, foundations, cells, lastError}) = do
    C.clearScreen
    C.setCursorPosition 0 28
    putStrLn "FreeCell"
    C.cursorDown 1 >> C.cursorForward 4
    sequence_ $ S.intersperse (C.cursorForward 2) $ renderMaybe <$> cells
    C.cursorForward 15
    sequence_ $ S.intersperse (C.cursorForward 2) $ renderMaybe <$>
        (S.mapWithIndex maybeCard foundations)
    C.cursorDownLine 1 >> C.cursorForward 4
    sequence_ $ S.mapWithIndex renderCascade' cascades
    if isJust lastError then do
        C.setCursorPosition 15 0
        setColor Nothing
        putStrLn $ fromJust lastError
    else return ()
    where
    renderCascade' :: Int -> Cascade -> IO ()
    renderCascade' n cascade = do
        setColor Nothing
        putStr $ "(" ++ show (n + 1) ++ ")"
        C.cursorDown 1 >> C.cursorBackward 3
        renderCascade cascade
        C.cursorUp 1 >> C.cursorForward 8


parseNum :: Int -> String -> Maybe Int
parseNum bound s = do
    n <- readMaybe s
    guard $ 0 < n && n <= bound
    return $ n - 1

execute :: [String] -> Configuration -> Either String Configuration

execute [from, to] conf = do
    f <- maybeToRight "Invalid command" $ parse from
    t <- maybeToRight "Invalid command" $ parse to
    maybeToRight "Invalid move" $ move f t conf
    where
        parse :: String -> Maybe CardPos
        parse "f" = Just Foundation
        parse "c" = Just AnyCell
        parse ('c' : i) = Cell <$> parseNum numOfCells i
        parse i = Cascade <$> parseNum numOfCascades i

execute [from, to, num] conf = do
    f <- maybeToRight "Invalid command" $ parseNum numOfCascades from
    t <- maybeToRight "Invalid command" $ parseNum numOfCascades to
    n <- maybeToRight "Invalid command" $ readMaybe num
    maybeToRight "Invalid move" $ moveToEmptyCascade f t n conf

execute _ _ = Left "Invalid command"

play :: Configuration -> IO ()
play conf = do
    render conf
    C.setCursorPosition 20 0
    setColor Nothing
    if all (==13) $ foundations conf then do
        putStrLn "Win!"
        putStr "Do it again? "
        hFlush stdout
        ans <- getLine
        if not (null ans) && toLower (head ans) == 'y' then main else return ()
    else do
        putStr "Move? "
        hFlush stdout
        cmd <- getLine
        play $ case execute (words $ toLower <$> cmd) conf of
            Left s -> conf { lastError = Just s }
            Right conf' -> (autoCollect conf') { lastError = Nothing }

main :: IO ()
main = do
    gen <- getStdGen
    let conf = deal gen
    play conf
