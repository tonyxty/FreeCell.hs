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

execute :: String -> Layout -> Either String Layout
execute command = go . words $ toLower <$> command
    where
    go :: [String] -> Layout -> Either String Layout
    go [f, t] layout = do
        from <- readMaybe f <?> "Where from?"
        to <- readMaybe t <?> "Where to?"
        first show $ move from to layout
    go [f, t, n] layout = do
        from <- readIndex numOfColumns f <?> "Where from?"
        to <- readIndex numOfColumns t <?> "Where to?"
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
