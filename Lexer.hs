module Lexer (LexemType(..), 
              Lexem(..), 
              lxName, 
              lxNumber,
              lxOp,  
              runLexer) where

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import System.IO
import Control.Monad.Loops

data LexemType = Indent
           | Dedent
           | Newline
           | LxName String
           | LxNumber Int
           | LxLiteral String
           | Keyword String
           | Op String
           | Comma
           | Semicolon
           | ParenthesisOpen
           | ParenthesisClose
           | BracketOpen
           | BracketClose
           | End
            deriving (Eq, Show) 

lxName (LxName s) = s
lxName _        = error "error"

lxNumber (LxNumber n) = n
lxNumber _          = error "error"


lxOp (Op s)     = s
lxOp _          = error "error"

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

move :: Int -> LexerState()
move n = modify (\st -> st { lsInput = drop n (lsInput st), lsLinePos = lsLinePos st + n } )

indent :: LexerState ()
indent = modify (\st -> st { lsResult = Lexem Indent (lsSrcLine st) (lsLinePos st) : lsResult st,
                             lsIndentStack = lsLinePos st : lsIndentStack st } )
                
dedent :: LexerState ()        
dedent = modify (\st -> st { lsResult = Lexem Dedent (lsSrcLine st) (lsLinePos st) : lsResult st,
                             lsIndentStack = drop 1 $ lsIndentStack st } )
dedentTo :: Int -> LexerState ()
dedentTo n = whileM_ (do { gets $ \st -> lsLinePos st < (head $ lsIndentStack st) } )  dedent

addLexem :: LexemType -> LexerStateData -> LexerState ()
addLexem t st0 = 
    modify (\st -> st { lsResult = l : lsResult st} )
    where l = Lexem t (lsSrcLine st0) (lsLinePos st0) 

nextLine :: LexerState ()
nextLine = do
    st <- get
    if ( (lxType $ head $ lsResult st) /= Newline) then addLexem Newline st else return ()
    modify (\st -> st { lsLinePos = 0, lsSrcLine = lsSrcLine st + 1 } )

manyC :: (Char -> Bool) -> LexerState String
manyC f = do
    st0 <- get
    let pref = takeWhile f $ lsInput st0
    move $ length pref
    return pref

isKnownSymbol :: Char -> Bool
isKnownSymbol = flip elem "<>=+-*/%$^|&!./|\\:"

isNameSymbol :: Char -> Bool 
isNameSymbol = liftA2 (||) isAlphaNum (=='_')

parseLine :: LexerState ()
parseLine = do
    st <- get 
    case lsInput st of
        ' ' : _ -> do { move 1; parseLine }
        '\n' : _ -> do { move 1; nextLine }
        _ -> do 
            if lsLinePos st > (head $ lsIndentStack st) then indent else  
                dedentTo $ lsLinePos st
            parseLineRest

parseLineRest :: LexerState ()
parseLineRest = do 
    st <- get
    if (lsInput st == "") then return () else do
        case head $ lsInput st of                 
             c | c == '\n' -> do 
                    move 1
                    nextLine
               | c == ' ' -> move 1
               | c == '(' -> do { move 1; addLexem ParenthesisOpen st }
               | c == ')' -> do { move 1; addLexem ParenthesisClose st }
               | c == ';' -> do { move 1; addLexem Semicolon st }
               | c == ',' -> do { move 1; addLexem Comma st }
               | isDigit c -> do { parseNumber }
               | isKnownSymbol c -> do { parseOperator }
               | isNameSymbol c -> do { parseName } 
               | otherwise -> do { parseOperator }
        if (head $ lsInput st) == '\n' then return () else parseLineRest

parseGen :: (Char -> Bool) -> (String -> LexemType) -> LexerState ()
parseGen fpred ctor = do
    st0 <- get
    s <- manyC fpred
    addLexem (ctor s) st0

parseName :: LexerState ()
parseName = parseGen isAlphaNum LxName

parseNumber :: LexerState ()
parseNumber = parseGen (liftA2 (||) isDigit (=='.')) (LxNumber . read) --(\s -> LxNumber (read s :: Integer) )

parseOperator :: LexerState ()
parseOperator = parseGen isKnownSymbol Op

parse :: LexerState ()
parse = do
    st <- get 
    case lsInput st of
        [] -> do { nextLine; dedentTo 0; addLexem End st }
        _ -> parseLine >> parse


runLexer :: String -> [Lexem]
runLexer s = 
    reverse $ lsResult $ snd $ fromJust $ res
    where res = runStateT parse $ LexerStateData [] s 0 0 [] [0]
    