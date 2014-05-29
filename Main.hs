module Main where

import Lexer
import Control.Monad
import Data.Functor
import Data.Maybe
import System.IO

main :: IO ()
main = do
    --h <- openFile "input.txt" ReadMode
    --cont <- hGetContents h
    cont <- join $ hGetContents `liftM` openFile "input.ysm" ReadMode
    print $ runLexer cont
