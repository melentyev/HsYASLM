module Parser (AST(..), runParser) where

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing 
    -fwarn-monomorphism-restriction -fwarn-hi-shadowing
    -fno-warn-unused-binds -fno-warn-unused-matches
#-}

import Lexer
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.Trans.State
import System.IO
import Control.Monad
import Control.Monad.Loops
import Control.Applicative ( (<*>), (<$>) )


data CompOp = Eq | NotEq | LessEq | GrEq | Less | Gr deriving (Show)

data AST = Module [AST]
           | Binding AST
           | TupleBinding [AST] AST
           | FuncBinding AST AST AST
           | Varargslist [AST]
           | Arg [AST]
           | Suite [AST]
           | IndentedSuite [AST] [AST]
           | CompoundExprStmt [AST]
           | ExprStmt AST (Maybe AST)
           | IfExpr AST AST (Maybe AST)
           | Test AST
           | Testlist [AST]
           | OrTest [AST]
           | AndTest [AST]
           | NotTest AST
           | Comparison AST CompOp AST
           | BtwOrExpr [AST]
           | XorExpr [AST]
           | AndExpr [AST]
           | ShiftExpr AST String AST
           | ArithExpr AST String AST
           | Term AST String AST
           | Factor String AST
           | Power AST [AST] (Maybe AST)
           | Lambdef AST AST -- varargslist '->' suite
           | Callarg AST
           | Name String
           | Scope AST
           | ConstInt Int
           | ConstFloat Double
           | ConstString String
            deriving (Show)


data ParserStateData = ParserStateData { 
    psInput       :: [Lexem],
    psLogged      :: [String]
} deriving (Show)

type ParserState a = StateT ParserStateData Maybe a

(<|>) :: ParserState a -> ParserState a -> ParserState a
(<|>) p1 p2 = do
    st <- get
    let res = runStateT p1 st -- :: Maybe (String, ParsingState)
    case res of 
        Just (val, st1) -> do { put st1; return val }
        Nothing -> p2

consume :: Int -> ParserState ()
consume n = modify (\st -> st { psInput = drop n (psInput st) } )

matchNextLexem :: (LexemType -> Bool) -> ParserState LexemType
matchNextLexem fpred = do
    lx <- gets $ lxType . head . psInput
    if fpred $ lx then do { consume 1; return lx } else mzero

manyOrNone :: ParserState a -> ParserState [a]
manyOrNone parser =
    let manyOrNone' acc = (do { pres <- parser; manyOrNone' (pres : acc) }) <|> return acc
    in manyOrNone' []    

many :: ParserState a -> ParserState [a]
many parser = do
    pres <- parser
    ((:) pres) `liftM` (manyOrNone parser)

separatedBy parser separator = do
    head <- parser
    tail <- manyOrNone (separator >> parser)
    return $ head : tail

wrappedBy parser (beg, end) = beg >> parser >>= (\r -> end >> return r)
    
oneOrNone :: ParserState a -> ParserState (Maybe a)
oneOrNone parser = do
    (do { pres <- parser; return $ Just pres }) <|> return Nothing

parse :: [AST] -> ParserState AST  
parse acc = Module `liftM` manyOrNone binding

    --finish <- gets $ not . null . psInput 
    --if finish then 
    --   (flip (:) acc) `liftM` binding >>= parse 
    --    else return $ Module acc


binding = tupleBinding <|> funcBinding
tupleBinding = do
    ids <- name `separatedBy` comma
    singleEq
    cont <- suite
    return $ TupleBinding (ids) cont

funcBinding = name >> varargslist >> singleEq >> suite
suite = indentedSuite <|> compoundExpr
indentedSuite = do
    newline >> indent 
    b <- binding
    e <- compoundExpr
    dedent
    return $ IndentedSuite [b] [e]

compoundExpr = whileStmt <|> forStmt <|> compoundExprStmt
whileStmt = keyword "while" >> test >> keyword "do" >> suite
forStmt = keyword "for" >> testlist >> keyword "in" >> testlist >> keyword "do" >> suite
compoundExprStmt = exprStmt
exprStmt = do 
    tl <- testlist 
    val <- oneOrNone (arrowLeft >> exprStmt)
    return $ ExprStmt tl val

testlist = Testlist <$> (test `separatedBy` comma)

test = lambdef <|> ifExpr <|> orTest
lambdef = backslash >> varargslist >> arrowRight >> suite
ifExpr = do 
    keyword "if" 
    cond <- test
    keyword "then"
    th <- suite
    keyword "else"
    el <- oneOrNone suite
    return $ IfExpr cond th el

orTest      = binaryOp andTest (keyword "or") OrTest
andTest     = binaryOp notTest (keyword "and") AndTest
notTest     = (NotTest <$> (keyword "not" >> notTest) ) <|> comparison
comparison  = samePriorBinaryOp btwOrExpr compOp Comparison
btwOrExpr   = binaryOp xorExpr (operator "|") BtwOrExpr
xorExpr     = binaryOp andExpr (operator "^") XorExpr
andExpr     = binaryOp shiftExpr (operator "&") AndExpr
shiftExpr   = samePriorBinaryOp arithExpr shiftOp ShiftExpr
arithExpr   = samePriorBinaryOp term termDelim ArithExpr
term        = samePriorBinaryOp factor factorDelim Term 
    
factor = prefixedFactor <|> power
{-prefixedFactor = do 
    op <- factorPrefixOp 
    val <- factor
    return $ Factor op val-}
prefixedFactor = Factor <$> factorPrefixOp <*> factor
power = Power <$> atom <*> manyOrNone trailer <*> oneOrNone (operator "**" >> factor)
{-power = do 
    a <- atom 
    tr <- manyOrNone trailer 
    pow <- oneOrNone (operator "**" >> factor)
    --error "here"
    return $ Power a tr pow --}
trailer = Scope <$> (operator "." >> name) <|> callarg
callarg = Callarg <$> atom
atom    =  testlist `wrappedBy` (parenthesisOpen, parenthesisClose) 
       <|> name 
       <|> constInt 
       <|> string


varargslist = many arg
arg = (Arg <$> (name `separatedBy` comma) ) <|> parentedArg
parentedArg = arg `wrappedBy` (parenthesisOpen, parenthesisClose)

string = (Name . lxName) <$> matchNextLexem (\lx -> case lx of LxName _ -> True; _ -> False)
name = (Name . lxName) <$> matchNextLexem (\lx -> case lx of LxName _ -> True; _ -> False)
constInt = (ConstInt . lxNumber) <$> matchNextLexem (\lx -> case lx of LxNumber _ -> True; _ -> False)
keyword expected = matchNextLexem (\lx -> case lx of LxName s | s == expected   -> True 
                                                              | otherwise       -> False
                                                     _                          -> False)
compOp =     (const Eq `liftM` (operator "==")) 
      <|> (const NotEq `liftM` (operator "/="))
      <|> (const Gr `liftM` (operator ">"))
      <|> (const GrEq `liftM` (operator ">="))
      <|> (const Less `liftM` (operator "<"))
      <|> (const LessEq `liftM` (operator "<="))
shiftOp =     (lxOp <$> (operator ">>>") ) 
          <|> (lxOp <$> (operator "<<<") ) 
termDelim =     (lxOp <$> (operator "+") ) 
            <|> (lxOp <$> (operator "-") ) 
factorDelim =     (lxOp <$> (operator "*") ) 
              <|> (lxOp <$> (operator "/") ) 
              <|> (lxOp <$> (operator "%") ) 
factorPrefixOp =     (lxOp <$> (operator "+") ) 
                 <|> (lxOp <$> (operator "-") ) 
                 <|> (lxOp <$> (operator "~") ) 

semicolon = matchNextLexem ((==) Semicolon)
comma = matchNextLexem ((==) Comma)
arrowLeft = matchNextLexem ((==) (Op "<-"))
arrowRight = matchNextLexem ((==) (Op "->"))
backslash = matchNextLexem ((==) (Op "\\"))
newline = matchNextLexem ((==) Newline) 
indent = matchNextLexem ((==) Indent) 
dedent = matchNextLexem ((==) Dedent)
parenthesisOpen = matchNextLexem ((==) ParenthesisOpen)
parenthesisClose = matchNextLexem ((==) ParenthesisClose)
singleEq = matchNextLexem ((==) (Op "="))
doubleEq = matchNextLexem ((==) (Op "=="))
haskellNotEq = matchNextLexem ((==) (Op "/="))
operator op = matchNextLexem ((==) (Op  op))
singlePipe = matchNextLexem ((==) (Op "|"))

binaryOp expr op ctor = 
    (\l -> if length l == 1 then head l else ctor l) `liftM` (expr `separatedBy` op)
    

samePriorBinaryOp expr opParser ctor = do 
    hd <- expr
    tail <- manyOrNone (do { op <- opParser; e <- expr; return (op, e) })
    return $ foldl (\acc (op, e) -> ctor acc op e ) hd tail

runParser :: [Lexem] -> AST
runParser ls = 
    fst $ fromJust $ res
    where res = runStateT (parse []) $ ParserStateData ls []

main :: IO ()
main = do
    cont <- join $ hGetContents `liftM` openFile "input.ysm" ReadMode
    -- mapM_ print $ runLexer cont
    print $ runParser $ runLexer cont
