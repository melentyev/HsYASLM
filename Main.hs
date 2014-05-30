module Main where

import Lexer
import Parser
import Control.Monad
import Data.Functor
import Data.Maybe
import System.IO

main :: IO ()
main = do
    cont <- join $ hGetContents `liftM` openFile "input.ysm" ReadMode
    print $ runParser $ runLexer cont