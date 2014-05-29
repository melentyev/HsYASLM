module Lexer (Lexem(..), runLexer) where

import Data.List
import Data.Maybe
import Control.Monad.Trans.State
import Control.Monad
import System.IO
import Data.Functor

data LexemType = Indent
           | Dedent
           | Newline
           | LxName
           | LxNumber Integer
           | LxString
           | Keyword String
           | Op String
            deriving (Eq, Show) 

data Lexem = Lexem {
    lxType      :: LexemType,
    lxSrcLine   :: Int,
    lxLinePos   :: Int
} deriving (Eq, Show) 

data LexerStateData = LexerStateData { 
    lsResult      :: [Lexem],
    lsInput       :: String,
    lsSrcLine     :: Int,
    lsLinePos     :: Int,
    lsLogged      :: [String],
    lsIndentStack :: [Int]
} deriving (Show)

type LexerState a = StateT LexerStateData Maybe a

record :: String -> LexerState ()
record s = modify (\st -> st { lsLogged = (lsLogged st) ++ [s] } )

move :: Int ->  LexerState()
move n = modify  (\st -> st { lsInput = drop 1 (lsInput st), lsLinePos = lsLinePos st  +1 } )


parseLine :: LexerState ()
parseLine = do
    st <- get 
    case lsInput st of
        ' ' : xs -> move 1
        xs -> parseLineRest
        
parseLineRest :: LexerState ()
parseLineRest = do 
    st <- get
    case lsInput st of 
        c : xs | isAlphaNum c -> do { parseName; parseLineRest }
               | c == '-' -> move 2

parseName :: LexerState ()
parseName = do
    st <- get
    case lsInput st of 
        c : xs | isAlphaNum c -> do { parseName; parseLineRest }
               | c == '-' -> move 2

parse :: LexerState ()
parse = do
    st <- get 
    case lsInput st of
        [] -> do return ()
        _ -> parseLine >> parse


runLexer :: String -> [Lexem]
runLexer s = 
    lsResult $ snd $ fromJust $ res
    where res = runStateT parse $ LexerStateData [] s 0 0 [] []
    

main :: IO ()
main = do
    --h <- openFile "input.txt" ReadMode
    --cont <- hGetContents h
    cont <- join $ hGetContents `liftM` openFile "input.ysm" ReadMode
    print $ runLexer cont
