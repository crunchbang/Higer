module TigerParseHelper (Program(..),
                         Exp(..),
                         LValue(..),
                         InfixOp(..),
                         FieldCreate(..),
                         Decl(..),
                         Type(..),
                         FieldDecl(..),
                         AST,
                         startParse,
                         MapEntry(..)
                        ) where

import Data.Map (Map)
import qualified Data.Map as Map

data MapEntry = SType Type
              | FType Type [Type]
              deriving (Show, Eq)

class AST a where
        parse :: a -> Map Id MapEntry -> (MapEntry, Map Id MapEntry)

startParse a = parse a (Map.insert "_" (SType (Type "NilVale")) (Map.empty))

type Id = String

data Program = Program Exp
             deriving (Eq)

instance Show Program where
        show (Program e) = "(Program " ++ show e ++ ")"

instance AST Program where
        parse (Program e) m = parse e m


data Exp = LExp LValue
         | NilValue
         | IntLiteral Int
         | StringLiteral String
         | SeqExp [Exp]
         | Negation Exp
         | CallExp { callFunId :: Id, callFunArgs :: [Exp] }
         | InfixExp { infixLhs :: Exp, op :: InfixOp, infixRhs :: Exp }
         | ArrCreate { arrType :: Type, size :: Exp, defVal :: Exp }
         | RecCreate { recType :: Type, recFields :: [FieldCreate] }
         | Assignment { assignmentLhs :: LValue, assignmentRhs :: Exp }
         | IfThen { ifCond :: Exp, thenExp :: Exp, elseExp :: Maybe Exp }
         | WhileExp { whileCond :: Exp, whileBody :: Exp }
         | ForExp { forVar :: Id, low :: Exp, high :: Exp, forBody :: Exp }
         | Break
         | LetExp { letDecl :: [Decl], letBody :: Exp }
         deriving (Eq)

instance Show Exp where
        show (LExp lv) = "(LExp " ++ show lv ++ ")"
        show NilValue = "(NilValue)"
        show (IntLiteral i) = "(IntLiteral (" ++ show i ++ "))"
        show (StringLiteral s) = "(StringLiteral (" ++ show s ++ "))"
        show (SeqExp xs) = "(SeqExp " ++ listToArgs xs ++ ")"
        show (Negation e) = "(Negation " ++ show e ++ ")"
        show (CallExp { callFunId = cfid, callFunArgs = cfargs }) = "(CallExp (" ++ show cfid ++ ") " ++ listToArgs cfargs ++ ")"
        show (InfixExp { infixLhs = il, op = op, infixRhs = ir }) = "(InfixExp " ++ show il ++ " " ++ show op ++ " " ++ show ir ++ ")"
        show (ArrCreate { arrType = at, size = sz, defVal = dv }) = "(ArrCreate " ++ show at ++ " " ++ show sz ++ " " ++ show dv ++ ")"
        show (RecCreate { recType = rt, recFields = rf }) = "(RecCreate " ++ show rt ++ " " ++ listToArgs rf ++ ")"
        show (Assignment { assignmentLhs = al, assignmentRhs = ar }) = "(Assignment " ++ show al ++ " " ++ show ar ++ ")"
        show (IfThen { ifCond = ic, thenExp = te, elseExp = ee }) = "(IfThen " ++ show ic ++ " " ++ show te ++ " " ++ show ee ++ ")"
        show (WhileExp { whileCond = wc, whileBody = wb }) = "(WhileExp " ++ show wc ++ " " ++ show wb ++ ")"
        show (ForExp { forVar = fv, low = l, high = h, forBody = fb }) = "(ForExp (" ++ show fv ++ ") " ++ show l ++ " " ++ show h ++ " " ++ show fb ++ ")"
        show (Break) = "(Break)"
        show (LetExp { letDecl = ld, letBody = lb }) = "(LetExp (LetDecl " ++ listToArgs ld ++ ") (LetBody " ++ show lb ++ "))"



parseSeq [e] m = parse e m
parseSeq (e:es) m = parseSeq es m'
                where (_, m') = parse e m
parseSeq [] m = error "Empty SeqExp"

checkTypes (ty1, m) (ty2, n) = if ty1 == ty2 then (ty1, m) else error ("Type mismatch:" ++ show ty1 ++ " " ++ show ty2)

checkFunCall cfid cfargs m = if cfargs == argTy then (SType retTy, m) else error ("Type mismatch" ++ show cfargs ++ " " ++ show argTy)
        where Just (FType retTy argTy) = Map.lookup cfid m

checkArgList [c] m = [ty]
        where (SType ty, _) = parse c m
checkArgList (c:cs) m = cty : (checkArgList cs m)
        where (SType cty, _) = parse c m

checkInfixTypes (leftTy, m) (rightTy, n) op | leftTy /= rightTy = error "Infix type error"
  | op `elem` [Add, Sub, Mul, Div]  = if leftTy == (SType (Type "int")) then (leftTy, m) else error "Infix error"
  | op `elem` [GreaterThanEqual, GreaterThan, LessThanEqual, LessThan] = if leftTy == (SType (Type "int")) || leftTy == (SType (Type "string")) then (leftTy, m) else error "Infix error"
  | op `elem` [Equal, NotEqual] = (leftTy, m)


contains [] y = True
contains (x:xs) y = elem x y && contains xs y

equals x y = contains x y && contains y x

fieldDec2List (f:fd) = (fId f, fType f) : fieldDec2List fd
fieldDec2List [] = []

fieldCreate2List (f:fc) m = (fieldId, fieldTy) : fieldCreate2List fc m
        where
                (FieldCreate fieldId _) = f
                (SType fieldTy, _) = parse f m
fieldCreate2List [] m = []

checkRecTypes (Type recName) rs m = if (equals (fieldDec2List fds) (fieldCreate2List rs m)) then (SType (Type recName), m) else error ("Record type error:" ++ show recName ++ " " ++ show rs ++ " " ++ show (fieldDec2List fds) ++ " " ++ show (fieldCreate2List rs m ))
        where
                (Just (SType (RecType fds))) = Map.lookup recName m


checkIfTypes ic te (Just ee) m = if icTy == (Type "int") && (teTy == eeTy) then (teTy, m) else error "IfCond type error"
        where
                (SType icTy, _) = parse ic m
                (teTy, _) = parse te m
                (eeTy, _) = parse ee m
checkIfTypes ic te Nothing m = if icTy == (Type "int") then (teTy, m) else error "IfCond type error"
        where
                (SType icTy, _) = parse ic m
                (teTy, _) = parse te m


checkWhileTypes wc wb m = if wcTy == (Type "int") then (wbTy, m) else error "WhileCond error"
        where
                (SType wcTy, _) = parse wc m
                (wbTy, _) = parse wb m

checkForTypes fv l h fb m = if lTy == (Type "int") && hTy == (Type "int") then (ty, m) else error "For type error"
        where
                (SType lTy, _) = parse l m
                (SType hTy, x) = parse h m
                m' = Map.insert fv (SType (Type "int")) m
                (ty, y) = parse fb m'

updateSymTab [lb] m = parse lb m
updateSymTab (l:lbs) m = updateSymTab lbs m'
                where (_, m') = parse l m
updateSymTab [] m = error "Empty Decl"

checkLetTypes ld lb m = (ty, m)
        where
                (_, m') = updateSymTab ld m
                (ty, _) = parse lb m'

instance AST Exp where
        parse (LExp lv) m = parse lv m
        parse (NilValue) m = (SType (Type "NilValue"), m)
        parse (IntLiteral _) m = (SType (Type "int"), m)
        parse (StringLiteral _) m = (SType (Type "string"), m)
        parse (SeqExp es) m = parseSeq es m
        parse (Negation e) m = checkTypes (parse e m) (SType (Type "int"), m)
        parse (CallExp { callFunId = cfid, callFunArgs = cfargs }) m = checkFunCall cfid (checkArgList cfargs m) m 
        parse (InfixExp { infixLhs = il, op = op, infixRhs = ir }) m = checkInfixTypes (parse il m) (parse ir m) op 
        --parse (ArrCreate { arrType = at, size = sz, defVal = dv }) m = checkTypes (SType at, m) (parse dv m) 
        parse (ArrCreate { arrType = at, size = sz, defVal = dv }) m = (SType at, m) 
        parse (RecCreate { recType = rt, recFields = rf }) m = checkRecTypes rt rf m
        parse (Assignment { assignmentLhs = al, assignmentRhs = ar }) m = checkTypes (parse al m) (parse ar m)
        parse (IfThen { ifCond = ic, thenExp = te, elseExp = ee }) m = checkIfTypes ic te ee m
        parse (WhileExp { whileCond = wc, whileBody = wb }) m = checkWhileTypes wc wb m
        parse (ForExp { forVar = fv, low = l, high = h, forBody = fb }) m = checkForTypes fv l h fb m
        parse (Break) m = (SType (Type "NilValue"), m)
        parse (LetExp { letDecl = ld, letBody = lb }) m = checkLetTypes ld lb m

data LValue = LVar Id
            | LSubscript LValue Exp
            | LField LValue Id
            deriving (Eq)

instance Show LValue where
        show (LVar iden) = "(LVar (" ++ show iden ++ "))"
        show (LSubscript lval e) = "(LSubscript " ++ show lval ++ " " ++ show e ++ ")"
        show (LField lval iden) = "(LField " ++ show lval ++ " (" ++ show iden ++ "))"

getFieldType (f:fd) iden = if (fId f == iden) then fType f else getFieldType fd iden
getFieldType [] iden = error "Fieldtype mismatch"

instance AST LValue where
        parse (LVar iden) m = (entry, m)
                where
                        (Just entry) = Map.lookup iden m
        parse (LSubscript lval e) m = if eTy == (Type "int") then (parse lval m) else error "LSubscript type error"
                where
                        (SType eTy, _) = parse e m
        parse (LField (LVar recName) iden) m = (SType idenTy, m)
                where
                        (Just (SType (Type recTy))) = Map.lookup recName m
                        (Just (SType (RecType fd))) = Map.lookup recTy m
                        idenTy = getFieldType fd iden

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
             deriving (Eq)

instance Show InfixOp where
        show Add = "(Add)"
        show Sub = "(Sub)"
        show Mul = "(Mul)"
        show Div = "(Div)"
        show Equal = "(Equal)"
        show NotEqual = "(NotEqual)"
        show GreaterThan = "(GreaterThan)"
        show LessThan = "(LessThan)"
        show GreaterThanEqual = "(GreaterThanEqual)"
        show LessThanEqual = "(LessThanEqual)"

data FieldCreate = FieldCreate Id Exp
                 deriving (Eq) 

instance Show FieldCreate where
        show (FieldCreate iden e) = "(FieldCreate (" ++ show iden ++ ") " ++ show e ++ ")"

instance AST FieldCreate where
        parse (FieldCreate id e) m = (parse e m)

data Decl = TypeDec { typeId :: Id, ty :: Type }
          | VarDec  { varId :: Id, varType :: Maybe Type, value :: Exp }
          | FunDec  { declFunId :: Id, declFunArgs :: [FieldDecl], funRetType :: Maybe Type, funDef :: Exp }
          deriving (Eq)

instance Show Decl where
        show TypeDec { typeId = ti, ty = t } = "(TypeDec (" ++ show ti ++ ") " ++ show t ++ ")"
        show VarDec  { varId = vi, varType = vt, value = v } = "(VarDec (" ++ show vi ++ ")  " ++ show vt ++ " " ++ show v ++ ")"
        show FunDec  { declFunId = dfid, declFunArgs = dfa, funRetType = frt, funDef = fd } = "(FunDec (" ++ show dfid ++ ") " ++ listToArgs dfa ++ " " ++ show frt ++ " " ++ show fd ++ ")"


isUnique id m = case (Map.lookup id m) of
                  Nothing -> True
                  _ -> False


checkVarDecTypes vi (Just vtTy) v m = if (isUnique vi m) && vtTy == vTy then (SType vTy, m') else error ("VarDec type error:" ++ show vtTy ++ " " ++ show vTy)
        where
                (SType vTy, _) = parse v m
                m' = Map.insert vi (SType vTy) m
checkVarDecTypes vi Nothing v m = if (isUnique vi m) then (vTy, m') else error "Duplicate declaration"
        where
                (vTy, _) = parse v m
                m' = Map.insert vi vTy m

getFieldDecTypeList (fd:fds) = fType fd : getFieldDecTypeList fds
getFieldDecTypeList [] = []

updateSymTabWithFun (fd:fds) m = updateSymTabWithFun fds m'
        where
                m' = Map.insert (fId fd) (SType (fType fd)) m
updateSymTabWithFun [] m = m

checkFunDecTypes dfid dfa (Just rtTy) fd m = if (isUnique dfid m) then (SType rtTy, n) else error "Duplicate fun definition error"
        where
                argTypes = getFieldDecTypeList dfa
                m' = updateSymTabWithFun dfa m
                (_, _) = parse fd m'
                n = Map.insert dfid (FType rtTy argTypes) m
checkFunDecTypes dfid dfa Nothing fd m = if (isUnique dfid m) then (SType (Type "NilValue"), n) else error "Duplicate fun definition error"
        where
                argTypes = getFieldDecTypeList dfa
                m' = updateSymTabWithFun dfa m
                (_, _) = parse fd m'
                n = Map.insert dfid (FType (Type "NilValue") argTypes) m
                

instance AST Decl where
        parse TypeDec { typeId = ti, ty = t } m = (SType t, Map.insert ti (SType t) m)
        parse VarDec  { varId = vi, varType = vt, value = v } m = checkVarDecTypes vi vt v m
        parse FunDec  { declFunId = dfid, declFunArgs = dfa, funRetType = frt, funDef = fd } m = checkFunDecTypes dfid dfa frt fd m

data Type = Type Id
          | ArrType Type
          | RecType [FieldDecl]
          deriving (Eq)

instance Show Type where
        show (Type iden) = "(Type (" ++ show iden ++ "))"
        show (ArrType iden) = "(ArrType " ++ show iden ++ ")"
        show (RecType fds) = "(RecType " ++ listToArgs fds ++ ")"

data FieldDecl = FieldDecl { fId :: Id, fType :: Type }
               deriving (Eq)

instance Show FieldDecl where
        show FieldDecl { fId = fiden, fType = ft } = "(FieldDecl (" ++ show fiden ++ ") " ++ show ft ++ ")"


listToArgs :: Show a => [a] -> String
listToArgs (x:xs) = (show x) ++ " " ++ (listToArgs xs)
listToArgs []     = ""

