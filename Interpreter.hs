module Interpreter (runInterpreter) where

import Lexer
import Parser
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.Trans.State
import System.IO
import Control.Monad
import qualified Data.Map as M
import Control.Monad.Loops
import Control.Applicative ( (<*>), (<$>) )
import qualified Text.Show.Pretty as Pr

type AppliedArgs = [Value]
data Value = ValueInt Int
           | ValueDouble Double
           | ValueString String
           | ValueFunc AST AppliedArgs -- FuncBinding, AppliedArgs
           | ValueVoid
           | ValueTuple [Value]
           | ValueModule ModuleBindings
           | Deferred Value
            deriving (Show)
valInt    (ValueInt x)    = x
valDouble (ValueDouble x) = x
valString (ValueString x) = x

opFromName c | c == "+" = (+) 
             | c == "-" = (-)
             | c == "*" = (*)
             | c == "/" = (/)
             | otherwise = error "here" 

data StackFrame = StackFrame {
    sfVars :: M.Map String Value
} deriving (Show)

data InterpreterStateData = InterpreterStateData { 
    isStack       :: [StackFrame],
    isLogged      :: [String]
} deriving (Show)


type InterpreterState a = StateT InterpreterStateData Maybe a

extract :: AST -> Value -> [ (String, Value) ]
extract (Name x) y               = [ (x, y) ]
extract (Arg []) (ValueTuple []) = []
extract (Arg [x]) y              = extract x y
extract (Arg (x : xs) ) (ValueTuple (y : ys ) ) = (extract x y) ++ (extract (Arg xs) (ValueTuple ys) )
extract _ _        = error "not arg"

pushStackFrame :: [AST] -> [Value] -> InterpreterState ()
pushStackFrame args vals = 
    if length args /= length vals then mzero else 
        let argsVals = concatMap id $ zipWith extract args vals
            dict = foldl (\m (a, v) -> M.insert a v m) M.empty argsVals
            frame = StackFrame dict
        in modify(\st -> st { isStack = (frame : isStack st) } )


eval :: AST -> InterpreterState Value
eval (ArithExpr l op r) = ValueInt <$> ( (opFromName op) <$> (valInt <$> eval l) <*> (valInt <$> eval r) )
eval (Term l op r) = ValueInt <$> ( (opFromName op) <$> (valInt <$> eval l) <*> (valInt <$> eval r) )
{-eval (ArithExpr l op r) = do
    l' <- eval l 
    r' <- eval r
    let op' = opFromName op
    return $ ValueInt $ op' (valInt l') (valInt r') -}
eval (Power a ( (Scope x) : xs) _) = scope
eval (Power a ( (CallArg x) : xs) _) = 
eval (Power a [] (Maybe f) ) = ValueInt <$> power <*> eval a <*> eval f
eval ( (FuncBinding _ (Varargslist args) code), vals) = do
    pushStackFrame args vals 
    eval code

eval x = x

runInterpreter (Module m) = do 
    let isMainFunc (FuncBinding (Name "map") (Varargslist []) _) = True
        isMainFunc _ = False
    let bind = find isMainFunc m
    runStateT (eval (bind, ValueVoid) ) (InterpreterStateData [] [])



main :: IO ()
main = do
    cont <- join $ hGetContents `liftM` openFile "input.ysm" ReadMode
    -- mapM_ print $ runLexer cont
    let lex = runLexer cont
    putStrLn $ Pr.ppShow $ lex
    let txt = Pr.ppShow $ runParser $ lex 
    putStrLn txt
