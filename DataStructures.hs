module DataStructures where


data CompOp = Eq | NotEq | LessEq | GrEq | Less | Gr deriving (Show, Eq)

keywords = ["if", "then", "else", "while", "do", "for", "in"]

data AST = Module [AST]
           | TupleBinding [AST] AST         -- NAME (',' NAME)* '=' suite
           | FuncBinding AST AST AST        -- NAME varargslist '=' suite
           | Varargslist [AST]
           | Arg [AST]
           | Suite [AST]
           | IndentedSuite [AST] [AST]
           | CompoundExprStmt [AST]
           | ExprStmt AST (Maybe AST)
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

astName (Name s) = s
astName _        = error "error"    

astFuncArgs (FuncBinding _ (Varargslist xs) _) = xs
astFuncArgs (Lambdef (Varargslist xs) _)       = xs

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


valInt    (ValueInt x)    = x
valDouble (ValueDouble x) = x
valString (ValueString x) = x