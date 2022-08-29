module Main where

import Util
import Deck
import FreeCell
import Render
import Data.Char
import Data.Foldable
import Data.Functor
import Data.Bifunctor
import Control.Monad
import Text.Read (readMaybe)
import System.Random
import System.IO

parseIndex :: Int -> String -> Maybe Int
parseIndex bound s = do
    n <- readMaybe s
    guard $ 0 < n && n <= bound
    return $ n - 1

parseFrom :: String -> Maybe CardFrom
parseFrom ('c':s) = Cell <$> parseIndex numOfCells s
parseFrom s = Column <$> parseIndex numOfColumns s

parseTo :: String -> Maybe CardTo
parseTo "c" = Just AnyCell
parseTo "f" = Just Foundation
parseTo s = Depot <$> parseIndex numOfColumns s

execute :: String -> Layout -> Either String Layout
execute command = go . words $ toLower <$> command
    where
    go :: [String] -> Layout -> Either String Layout
    go [f, t] layout = do
        from <- parseFrom f <?> "Where from?"
        to <- parseTo t <?> "Where to?"
        first show $ move from to layout
    go [f, t, n] layout = do
        from <- parseIndex numOfColumns f <?> "Where from?"
        to <- parseIndex numOfColumns t <?> "Where to?"
        n <- readMaybe n <?> "How many cards?"
        first show $ moveToVacancy from to n layout
    go _ _ = Left "Invalid command"

play :: Layout -> IO ()
play layout = do
    render layout
    layout <- doUntilM $ do
        cursorToMessages
        putStrClearLine "Move? "
        command <- getLine
        case execute command layout of
            Left e -> do
                cursorToErrors
                putStrClearLine e
                return Nothing
            Right layout -> return $ Just layout
    unless (win layout) . play . autoCollect $ layout

main :: IO ()
main = doWhileM_ $ do
    gen <- getStdGen
    play . makeLayout . deal $ gen
    cursorToMessages
    putStrLn "Win!"
    putStr "Do it again? "
    hFlush stdout
    ans <- getLine
    return $ not (null ans) && toLower (head ans) == 'y'
