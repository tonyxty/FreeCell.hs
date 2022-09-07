{-# LANGUAGE NamedFieldPuns, MonadComprehensions #-}
module Render where

import Deck
import FreeCell
import Data.Maybe
import Data.Functor
import qualified Data.Sequence as S
import Control.Monad
import Text.Read
import System.IO
import qualified System.Console.ANSI as C

strCard :: Card -> String
strCard card = "♠♥♦♣" !! fromEnum (suit card) : ["A23456789TJQK" !! (rank card - 1)]

setColor :: C.Color -> IO ()
setColor color = C.setSGR [C.SetColor C.Foreground C.Dull color]

renderColor :: Color -> C.Color
renderColor Red = C.Red
renderColor Black = C.White

normalColor :: C.Color
normalColor = C.White

errorColor :: C.Color
errorColor = C.Red

renderCard :: Card -> IO ()
renderCard card = setColor (renderColor . color $ card) >> putStr (strCard card)

renderMaybe :: Maybe Card -> IO ()
renderMaybe Nothing = setColor normalColor >> putStr "[  ]"
renderMaybe (Just card) = putChar ' ' >> renderCard card >> putChar ' '

renderColumn :: Column -> IO ()
renderColumn column = do
    -- bug (in ANSI terminal standard?):
    -- C.cursorDown 0 will cause the cursor to move down by 1 line
    let l = length column
    unless (l == 0) $ C.cursorDown l
    mapM_ (\ card -> renderCard card >> C.cursorUp 1 >> C.cursorBackward 2) column

render :: Layout -> IO ()
render Layout {columns, foundations, cells} = do
    C.clearScreen
    C.setCursorPosition 0 28
    putStrLn "FreeCell"
    C.cursorDown 1 >> C.cursorForward 4
    sequence_ . S.intersperse (C.cursorForward 3) $ renderMaybe <$> cells
    C.cursorForward 10
    sequence_ . S.intersperse (C.cursorForward 3) $ renderMaybe <$> S.mapWithIndex maybeCard foundations
    C.cursorDownLine 1 >> C.cursorForward 4
    sequence_ $ S.mapWithIndex renderColumn' columns
    where
    renderColumn' :: Int -> Column -> IO ()
    renderColumn' n cascade = do
        setColor normalColor
        putStr $ "(" ++ show (n + 1) ++ ")"
        C.cursorDown 1 >> C.cursorBackward 3
        renderColumn cascade
        C.cursorUp 1 >> C.cursorForward 8

strError :: Error -> String
strError NoCard = "No card to move"
strError Unacceptable = "Can't move"
strError Vacancy = "Moving to vacancy (requires number of cards)"
strError (TooLong n) = "Need to move " ++ show n ++ " cards"

renderError :: Error -> IO ()
renderError e = do
    C.setCursorPosition 15 0
    setColor normalColor
    putStrLn $ strError e

cursorToMessages :: IO ()
cursorToMessages = C.setCursorPosition 20 0 >> setColor normalColor

cursorToErrors :: IO ()
cursorToErrors = C.setCursorPosition 15 0 >> setColor errorColor

putStrClearLine :: String -> IO ()
putStrClearLine s = do
    putStr s
    C.clearFromCursorToLineEnd
    hFlush stdout

pIndex :: Int -> ReadPrec Int
pIndex n = [ i-1 | i <- readPrec, 0 < i && i <= n ]

readIndex :: Int -> String -> Maybe Int
readIndex n s = [ i | (i, "") <- listToMaybe . readPrec_to_S (pIndex n) minPrec $ s ]

pChar :: Char -> ReadPrec Char
pChar c = [ x | x <- get, x == c ]

instance Read CardFrom where
    readPrec = ((pChar 'c' $> Cell) <*> pIndex numOfCells)
        <++ (Column <$> pIndex numOfColumns)

instance Read CardTo where
    readPrec = (pChar 'c' $> AnyCell)
        <++ (pChar 'f' $> Foundation)
        <++ (Depot <$> pIndex numOfColumns)
