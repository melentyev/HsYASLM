module Interpreter (runInterpreter) where

import DataStructures
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

voidType   = Type Nothing [] []
intType    = Type Nothing [] []
funcType   = Type Nothing [] []
doubleType = Type Nothing [] []
tupleType  = Type Nothing [] []
listType   = Type Nothing [] []

valueVoid  = Value voidType ValueVoid
valueInt   = (Value intType) . ValueInt
valueList  = (Value listType) . ValueList
valueTuple = (Value tupleType) . ValueTuple

opFromName c | c == "+" = (+) 
             | c == "-" = (-)
             | c == "*" = (*)
             -- | c == "/" = (/)
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
extract (Arg []) (Value _ (ValueTuple [] ) ) = []
extract (Arg [x]) y              = extract x y
extract (Arg (x : xs) ) (Value t (ValueTuple (y : ys ) ) ) = 
    extract x y ++ extract (Arg xs) (Value t (ValueTuple ys) ) 
extract _ _        = error "not arg"

pushFrame :: [AST] -> [Value] -> InterpreterState ()
pushFrame args vals = 
    if length args /= length vals then mzero else 
        let argsVals = concatMap id $ zipWith extract args vals
            dict = foldl (\m (a, v) -> M.insert a v m) M.empty argsVals
            frame = StackFrame dict
        in modify(\st -> st { isStack = (frame : isStack st) } )

popFrame = modify(\st -> st { isStack = drop 1 $ isStack st } )

basicBinaryOperation :: String -> Value -> Value -> Value
basicBinaryOperation op (Value t (ValueInt l)) (Value _ (ValueInt r)) = 
    valueInt $ (opFromName op) l r

findVariable :: String -> InterpreterState (Maybe Value)
findVariable nm = do
    frame <- (head . isStack) <$> get
    return $ nm `M.lookup` (sfVars frame)
    

addLocalVar :: String -> Value -> InterpreterState ()
addLocalVar nm val = 
    let helper st = 
            let hd  = sfVars $ head $ isStack st 
                hd' = StackFrame $ M.insert nm val hd
            in st { isStack = hd' : (isStack st)}
    in modify helper

evalFunction :: Value -> InterpreterState Value
evalFunction fn@(Value t (ValueFunc ast applied ) ) 
    | (length $ astFuncArgs ast) == length applied = do
        pushFrame (astFuncArgs ast) (reverse applied)
        case ast of { FuncBinding (Name nm) _ _ | nm == "inc" -> error $ "here" ++ show ast; _ -> return (); }
        res <- eval (astCodeBody ast)
        popFrame >> return res
        
    | otherwise = return fn
evalFunction x = error $ show x


evalTrailer [] fn@(Value _ (ValueFunc _ _ )) = evalFunction fn
evalTrailer (Callarg arg : xs ) (Value t (ValueFunc ast applied )) = do
    a <- eval arg
    evalTrailer xs (Value t (ValueFunc ast (a : applied) ))
evalTrailer (Scope (Name nm) : xs) v = undefined
evalTrailer [] v = return v

evalPower :: Maybe AST -> Value -> InterpreterState Value
evalPower Nothing  v    = return v
evalPower (Just p) v    = return v

eval :: AST -> InterpreterState Value
eval (ConstInt n)       = return $ Value intType $ ValueInt n
--eval (ConstDouble n)    = return $ Value doubleType $ ValueDouble n
eval (ArithExpr l op r) = basicBinaryOperation op <$> eval l <*> eval r
eval (Term l op r)      = basicBinaryOperation op <$> eval l <*> eval r
eval (Power a tr p)     = eval a >>= evalTrailer tr >>= evalPower p
eval (Name nm)          = do 
    var <- findVariable nm
    --maybe mzero return <$> findVariable nm
    case var of 
        Just val -> return val
        Nothing  -> mzero
eval ast@(FuncBinding (Name nm) (Varargslist args) _) = do
    addLocalVar nm (Value funcType (ValueFunc ast []) )
    return $ valueVoid

eval (Testlist l)           = do  
    res <- mapM (eval) l
    return $ if length res > 1 then valueTuple res else head res
eval (CompoundExprStmt s) = do
    st <- get
    error $ "here" ++ show st
    mapM_ (eval) (init s) >> eval (last s)
eval (Suite s)            = mapM_ (eval) (init s) >> eval (last s)
eval (Module m)           = mapM_ (eval) m >> return valueVoid
eval _ = mzero

runMainModule m@(Module mod) = do 
    pushFrame [] []
    eval m
    main <- findVariable "main"
    case main of 
        Just (Value t (ValueFunc ast _) ) -> evalFunction $ Value t (ValueFunc ast [valueVoid])
        _ -> mzero
    

runInterpreter m = runStateT (runMainModule m) (InterpreterStateData [] [])
    



main :: IO ()
main = do
    cont <- join $ hGetContents `liftM` openFile "input.ysm" ReadMode
    -- mapM_ print $ runLexer cont
    let lex = runLexer cont
    putStrLn $ Pr.ppShow $ lex
    let ast = runParser $ lex 
    putStrLn $ Pr.ppShow $ ast
    let res = runInterpreter ast
    putStrLn $ Pr.ppShow $ res
