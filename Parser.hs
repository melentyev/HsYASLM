module Parser (AST(..), runParser, CompOp, keywords) where

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing 
    -fwarn-monomorphism-restriction -fwarn-hi-shadowing
    -fno-warn-unused-binds -fno-warn-unused-matches
#-}
import DataStructures
import Lexer
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.Trans.State
import System.IO
import Control.Monad
import Control.Monad.Loops
import Control.Applicative ( (<*>), (<$>) )
import qualified Text.Show.Pretty as Pr


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
    st <- get
    if (length $ psInput st) < 1 then error "matchNextLexem list finished unexpected" else return ()
    lx <- gets $ lxType . head . psInput
    if fpred $ lx then do { consume 1; return lx } else mzero

manyOrNone :: ParserState a -> ParserState [a]
manyOrNone parser =
    let manyOrNone' acc = (do { pres <- parser; manyOrNone' (pres : acc) }) <|> return acc
    in reverse <$> manyOrNone' []    

many :: ParserState a -> ParserState [a]
many parser = (:) <$> parser <*> manyOrNone parser
{- many parser = do
    pres <- parser
    ((:) pres) `liftM` (manyOrNone parser) -}

separatedBy parser separator = do
    head <- parser
    tail <- manyOrNone (separator >> parser)
    return $ head : tail

wrappedBy parser (beg, end) = beg >> parser >>= (\r -> end >> return r)
followedBy parser by = parser >>= (\r -> by >> return r)
    
oneOrNone :: ParserState a -> ParserState (Maybe a)
oneOrNone parser = do
    (do { pres <- parser; return $ Just pres }) <|> return Nothing

parse :: [AST] -> ParserState AST  
parse acc = Module `liftM` manyOrNone binding

    --finish <- gets $ not . null . psInput 
    --if finish then 
    --   (flip (:) acc) `liftM` binding >>= parse 
    --    else return $ Module acc


binding = (newline >> binding) <|> tupleBinding <|> funcBinding
tupleBinding = TupleBinding <$> (name `separatedBy` comma) <*> (operator "=" >> suite)

--funcBinding = name >> varargslist >> operator "=" >> suite
funcBinding = FuncBinding <$> name <*> (varargslist `followedBy` operator "=") <*> suite
suite = indentedSuite <|> compoundExpr
indentedSuite = IndentedSuite <$> (newline >> indent >> manyOrNone binding)
                              <*> (many compoundExpr `followedBy` dedent)

compoundExpr = (newline >> compoundExpr) <|> whileStmt <|> forStmt <|> compoundExprStmt
whileStmt = keyword "while" >> test >> keyword "do" >> suite
forStmt = keyword "for" >> testlist >> keyword "in" >> testlist >> keyword "do" >> suite
compoundExprStmt =  CompoundExprStmt 
                <$> exprStmt `separatedBy` semicolon 
                `followedBy` oneOrNone semicolon 
                `followedBy` newline
exprStmt = assignStmt <|> testlist
assignStmt = AssignStmt <$> testlist  <*> (arrowLeft >> assignStmt)
testlist = Testlist <$> (test `separatedBy` comma)

test = lambdef 
     <|> ifExpr 
     <|> cons
lambdef = Lambdef <$> (backslash >> varargslist `followedBy` arrowRight) <*> suite
ifExpr = do 
    keyword "if"
    cond <- test
    keyword "then"
    th <- exprStmt <|> suite 
    keyword "else"
    el <- oneOrNone (exprStmt <|> suite)
    return $ IfExpr cond th el
cons = (\l r -> maybe l (Cons l) r ) <$> orTest <*> oneOrNone (operator "::" >> cons) 

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
prefixedFactor = Factor <$> factorPrefixOp <*> factor
power = Power <$> atom <*> manyOrNone trailer <*> oneOrNone (operator "**" >> factor)
trailer = (Scope <$> (operator "." >> name) ) <|> callarg
callarg = Callarg <$> atom
atom    =  testlist `wrappedBy` (parenthesisOpen, parenthesisClose) 
       <|> name 
       <|> constInt 
       -- <|> string


varargslist = Varargslist <$> many arg
arg = (Arg <$> (name `separatedBy` comma) ) <|> parentedArg
parentedArg = arg `wrappedBy` (parenthesisOpen, parenthesisClose)

string = (ConstString . lxName) <$> matchNextLexem (\lx -> case lx of LxName _ -> True; _ -> False)
name = (Name . lxName) <$> matchNextLexem (\lx -> 
        case lx of LxName s | s `notElem` keywords -> True
                            | otherwise            -> False
                   _                               -> False)
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
    let lex = runLexer cont
    let res = runParser $ lex
    putStrLn $ Pr.ppShow $ lex
    let txt = Pr.ppShow $ res :: String
    putStrLn txt
