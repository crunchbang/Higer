module TigerParseHelper (Program(..),
                         Exp(..),
                         LValue(..),
                         InfixOp(..),
                         FieldCreate(..),
                         Decl(..),
                         Type(..),
                         FieldDecl(..),
                        ) where

type Id = String

data Program = Program Exp
             deriving (Eq)

instance Show Program where
        show (Program e) = "(Program " ++ show e ++ ")"



data Exp = LExp LValue
         | NilValue
         | IntLiteral Int
         | StringLiteral String
         | SeqExp [Exp]
         | Negation Exp
         | CallExp { callFunId :: Id, callFunArgs :: [Exp] }
         | InfixExp { infixLhs :: Exp, op :: InfixOp, infixRhs :: Exp }
         | ArrCreate { arrType :: Id, size :: Exp, defVal :: Exp }
         | RecCreate { recType :: Id, recFields :: [FieldCreate] }
         | Assignment { assignmentLhs :: LValue, assignmentRhs :: Exp }
         | IfThen { ifCond :: Exp, thenExp :: Exp, elseExp :: Maybe Exp }
         | WhileExp { whileCond :: Exp, whileBody :: Exp }
         | ForExp { forVar :: Id, low :: Exp, high :: Exp, forBody :: Exp }
         | Break
         | LetExp { letDecl :: [Decl], letBody :: [Exp] }
         deriving (Eq)

instance Show Exp where
        show (LExp lv) = "(LExp " ++ show lv ++ ")"
        show NilValue = "(NilValue)"
        show (IntLiteral i) = "(IntLiteral " ++ show i ++ ")"
        show (StringLiteral s) = "(StringLiteral " ++ show s ++ ")"
        show (SeqExp xs) = "(SeqExp " ++ show xs ++ ")"
        show (Negation e) = "(Negation " ++ show e ++ ")"
        show (CallExp { callFunId = cfid, callFunArgs = cfargs }) = "(CallExp " ++ show cfid ++ " " ++ show cfargs ++ ")"
        show (InfixExp { infixLhs = il, op = op, infixRhs = ir }) = "(InfixExp " ++ show il ++ " " ++ show op ++ " " ++ show ir ++ ")"
        show (ArrCreate { arrType = at, size = sz, defVal = dv }) = "(ArrCreate " ++ show at ++ " " ++ show sz ++ " " ++ show dv ++ ")"
        show (RecCreate { recType = rt, recFields = rf }) = "(RecCreate " ++ show rt ++ " " ++ show rf ++ ")"
        show (Assignment { assignmentLhs = al, assignmentRhs = ar }) = "(Assignment " ++ show al ++ " " ++ show ar ++ ")"
        show (IfThen { ifCond = ic, thenExp = te, elseExp = ee }) = "(IfThen " ++ show ic ++ " " ++ show te ++ " " ++ show ee ++ ")"
        show (WhileExp { whileCond = wc, whileBody = wb }) = "(WhileExp " ++ show wc ++ " " ++ show wb ++ ")"
        show (ForExp { forVar = fv, low = l, high = h, forBody = fb }) = "(ForExp " ++ show fv ++ " " ++ show l ++ " " ++ show h ++ " " ++ show fb ++ ")"
        show (Break) = "(Break)"
        show (LetExp { letDecl = ld, letBody = lb }) = "(LetExp " ++ show ld ++ " " ++ show lb ++ ")"

data LValue = LVar Id
            | LSubscript LValue Exp
            | LField LValue Id
            deriving (Show, Eq)

data InfixOp = Add
             | Sub
             | Mul
             | Div
             | Equal
             | NotEqual
             | GreaterThan
             | LessThan
             | GreaterThanEqual
             | LessThanEqual
             deriving (Show, Eq)

data FieldCreate = FieldCreate Id Exp
                 deriving (Show, Eq) 

data Decl = TypeDec { typeId :: Id, ty :: Type }
          | VarDec  { varId :: Id, varType :: Maybe Id, value :: Exp }
          | FunDec  { declFunId :: Id, declFunArgs :: [FieldDecl], funRetType :: Maybe Id, funDef :: Exp }
          deriving (Show, Eq)

data Type = Type Id
          | ArrType Id
          | RecType FieldDecl
          deriving (Show, Eq)

data FieldDecl = FieldDecl { fId :: Id, fType :: Id }
               deriving (Show, Eq)

-- Helper functions for displaying the tree
