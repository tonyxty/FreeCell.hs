{-# LANGUAGE NamedFieldPuns #-}
module Render where

import Deck
import FreeCell
import qualified Data.Sequence as S
import Control.Monad
import qualified System.Console.ANSI as C

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
renderCard card = setColor (renderColor . color $ card) >> putStr (show card)

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

renderError :: Error -> IO ()
renderError e = do
    C.setCursorPosition 15 0
    setColor normalColor
    print e

cursorToMessages :: IO ()
cursorToMessages = C.setCursorPosition 20 0 >> setColor normalColor

cursorToErrors :: IO ()
cursorToErrors = C.setCursorPosition 15 0 >> setColor errorColor

clearLine :: IO ()
clearLine = C.clearFromCursorToLineEnd
