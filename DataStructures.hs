module DataStructures where


data CompOp = Eq | NotEq | LessEq | GrEq | Less | Gr deriving (Show, Eq)

keywords :: [String]
keywords = ["if", "then", "else", "while", "do", "for", "in"]

data AST = Module [AST]
           | TupleBinding [AST] AST         -- NAME (',' NAME)* '=' suite
           | FuncBinding AST AST AST        -- NAME varargslist '=' suite
           | Varargslist [AST]
           | Arg [AST]
           | Suite [AST]
           | IndentedSuite [AST] [AST]
           | CompoundExprStmt [AST]
           | AssignStmt AST AST
           | IfExpr AST AST (Maybe AST)
           | Test AST
           | Testlist [AST]
           | Cons AST AST
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
           | Lambdef AST AST                -- '\' varargslist '->' suite
           | Callarg AST
           | Name String
           | Scope AST
           | ConstInt Int
           | ConstDouble Double
           | ConstString String
            deriving (Show, Eq)

astName :: AST -> String
astName (Name s) = s
astName _        = error "error"    

astFuncArgs :: AST -> [AST]
astFuncArgs (FuncBinding _ (Varargslist xs) _) = xs
astFuncArgs (Lambdef (Varargslist xs) _)       = xs

astCodeBody :: AST -> AST
astCodeBody (FuncBinding _ _ x) = x
astCodeBody (Lambdef _  x)      = x

type AppliedArgs = [Value]
data Instance = ValueInt Int
           | ValueDouble Double
           | ValueString String
           | ValueFunc AST AppliedArgs -- FuncBinding, AppliedArgs
           | ValueVoid
           | ValueTuple [Value]
           | ValueList [Value]
           | Deferred Value
            deriving (Show, Eq)

data Type = Type {
    tBase           :: Maybe Type,
    tMethods        :: [String],
    tStaticMethods  :: [String]
} deriving (Show, Eq)

data Value = Value {
    vType :: Type,
    vInst :: Instance
} deriving (Show, Eq)

valInt :: Instance -> Int
valInt    (ValueInt x)    = x

valDouble :: Instance -> Double
valDouble (ValueDouble x) = x

valString :: Instance -> String
valString (ValueString x) = x