import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty, delete, fromList)
import Debug.Trace
import Value

import Data.Bits

--
-- Evaluate functions
--
evalExpr :: StateT -> Expression -> StateTransformer Value
-- VarRef
evalExpr env (VarRef (Id id)) = stateLookup env id

-- IntLit
evalExpr env (IntLit int) = return $ Int int

-- StringLit
evalExpr env (StringLit string) = return $ String string

-- BoolLit
evalExpr env (BoolLit bool) = return $ Bool bool

-- InfixExpr
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    case v1 of
        (Return va) -> do
            case v2 of
                (Return vb) -> infixOp env op va vb
                _ -> infixOp env op va v2
        _ -> do
            case v2 of
                (Return vb) -> infixOp env op v1 vb
                _ -> infixOp env op v1 v2

-- AssignExpr OpAssign LVar
evalExpr env (AssignExpr OpAssign lValue expr) = do
    case lValue of
        (LVar var) -> do
            tent <- stateLookup env var
            e <- evalExpr env expr
            case tent of
                GlobalVar -> createGlobalVar var e
                _ -> setVar var e
        (LBracket expr1 expr2) -> do
            case expr1 of
                VarRef (Id id) -> do
                    evaluedI <- stateLookup env id
                    pos <- evalExpr env expr2
                    e <- evalExpr env expr
                    case evaluedI of
                        Array l -> do
                            newArray <- setVarArray (Array []) (Array l) pos e
                            setVar id newArray
                        _ -> error $ "nao e um array"

-- NumLit
evalExpr env (NumLit double) = return $ Double double

-- UnaryAssignExpr
evalExpr env (UnaryAssignExpr inc (LVar var)) = do
    case inc of
        PrefixInc -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1)))
        PrefixDec -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))
        PostfixInc -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1)))
        PostfixDec -> evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr OpSub (VarRef (Id var)) (IntLit 1)))

-- PrefixExpr
evalExpr env (PrefixExpr op expr) = do
    v <- evalExpr env expr
    prefixOp env op v

-- CallExpr
evalExpr env (CallExpr name params) = do
    case name of
        DotRef expr (Id id) -> do
            array <- evalExpr env expr
            case array of
                Array l -> do
                    case id of
                        "concat" -> myConcat env l params
                        "len" -> return (myLength 0 l)
                        "head" -> return (head l)
                        "tail" -> return (Array (tail l))
                        "equals" -> myEquals env l params
        _ -> do
            evaluedName <- evalExpr env name
            case evaluedName of
                Function id args stmt -> do
                    pushScope
                    showArgs env args params
                    ret <- evalStmt env (BlockStmt stmt)
                    popScope
                    case ret of
                        Return r -> return r
                        Break b -> error $ "break no lugar errado"
                        _ -> return Nil

-- CondExpr 	
evalExpr env (CondExpr expr1 expr2 expr3) = do
    evaluedExpr <- evalExpr env expr1
    case evaluedExpr of
        Bool True -> evalExpr env expr2
        Bool False -> evalExpr env expr3

-- ArrayLit
evalExpr env (ArrayLit []) = return (Array [])
evalExpr env (ArrayLit l) = evalArray env l (Array [])

-- ListExpr
evalExpr env (ListExpr []) = return Nil
evalExpr env (ListExpr (l:ls)) = do
    evalExpr env l >> evalExpr env (ListExpr ls)

-- DotRef
evalExpr env (DotRef expr (Id id)) = do
    array <- evalExpr env expr
    case array of
        Array l -> do
            case id of
                "len" -> return (myLength 0 l)
                "head" -> return (head l)
                "tail" -> return (Array (tail l))
                _ -> error $ "Funcao nao existente"

-- BracketRef
evalExpr env (BracketRef expr1 expr2) = do
    evaluedExpr1 <- evalExpr env expr1
    evaluedExpr2 <- evalExpr env expr2
    evalElementAt env evaluedExpr1 evaluedExpr2

-- compara se a chamada tem mesmo numero de parametros e cria as variaveis 
showArgs :: StateT-> [Id]-> [Expression]-> StateTransformer Value
showArgs env [] [] = return Nil
showArgs env ((Id name):ids) (param:params) =  do
        evaluatedParam <- evalExpr env param
        createLocalVar name evaluatedParam
        showArgs env ids params
showArgs env _ _ = error $ "Numero de parametros não sao iguais ao da funcao"

evalStmt :: StateT -> Statement -> StateTransformer Value
-- EmptyStmt
evalStmt env EmptyStmt = return Nil

-- VarDeclStmt
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)

-- ExprStmt
evalStmt env (ExprStmt expr) = evalExpr env expr

-- BlockStmt
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (stmt:stmts)) = do
    evaluedStmt <- evalStmt env stmt
    case evaluedStmt of
        Break b -> return (Break b)
        Return r -> return (Return r)
        _ -> evalStmt env (BlockStmt stmts)

-- IfSingleStmt
evalStmt env (IfSingleStmt expr stmt) = do
    evaluedExpr <- evalExpr env expr
    case evaluedExpr of
        Bool True -> evalStmt env stmt
        Bool False -> return Nil

-- IfStmt
evalStmt env (IfStmt expr stmt1 stmt2) = do
    evaluedExpr <- evalExpr env expr
    case evaluedExpr of
        Bool True -> evalStmt env stmt1
        Bool False -> evalStmt env stmt2

-- WhileStmt
evalStmt env (WhileStmt expr stmt) = do
    evaluedExpr <- evalExpr env expr
    case evaluedExpr of
        Bool True -> do
            evaluedStmt <- evalStmt env stmt
            case evaluedStmt of
                Break b -> return Nil
                Return r -> return (Return r)
                _ -> evalStmt env (WhileStmt expr stmt)
        Bool False -> return Nil

-- DoWhileStmt
evalStmt env (DoWhileStmt stmt expr) = do
    evaluedExpr <- evalExpr env expr
    evaluedStmt <- evalStmt env stmt
    case evaluedExpr of
        Bool True -> do
            case evaluedStmt of
                Break b -> return Nil
                Return r -> return (Return r)
                _ -> evalStmt env (DoWhileStmt stmt expr)
        Bool False -> return Nil

-- ForStmt
evalStmt env (ForStmt init test inc stmt) = do
    case init of
        NoInit -> return Nil
        VarInit vars -> varDeclList env vars
        ExprInit expr -> evalExpr env expr
    case test of
        Nothing -> do
            evaluedStmt <- evalStmt env stmt
            case evaluedStmt of
                Break b -> return Nil
                Return r -> return (Return r)
                _ -> do
                    case inc of
                        Nothing -> evalStmt env (ForStmt NoInit test inc stmt)
                        Just incM -> evalExpr env incM >> evalStmt env (ForStmt NoInit test inc stmt)
        Just testM -> do
            evaluedExpr <- evalExpr env testM
            case evaluedExpr of
                Bool True -> do
                    evaluedStmt <- evalStmt env stmt
                    case evaluedStmt of
                        Break b -> return Nil
                        Return r -> return (Return r)
                        _ -> do
                            case inc of
                                Nothing -> evalStmt env (ForStmt NoInit test inc stmt)
                                Just incM -> evalExpr env incM >> evalStmt env (ForStmt NoInit test inc stmt)
                Bool False -> return Nil

-- BreakStmt
evalStmt env (BreakStmt id) = do
    case id of
        Nothing -> return (Break Nothing)
        Just idM -> return (Break (Just idM))

-- FuncStmt
evalStmt env (FunctionStmt (Id name) args stmts) = createGlobalVar name (Function (Id name) args stmts)

-- ReturnStmt
evalStmt env (ReturnStmt expr) = do
    case expr of
        Nothing -> return (Return Nil)
        Just exprM -> do
            evaluedExpr <- evalExpr env exprM
            return (Return evaluedExpr)

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2
infixOp env OpLShift (Int v1) (Int v2) = return $ Int  $ shift v1 v2
infixOp env OpSpRShift (Int v1) (Int v2) = return $ Int  $ shiftR v1 v2
infixOp env OpBAnd (Int v1) (Int v2) = return $ Int $ ((.&.) v1 v2)
infixOp env OpBOr (Int v1) (Int v2) = return $ Int $ ((.|.) v1 v2)
infixOp env OpBXor (Int v1) (Int v2) = return $ Int $ (xor v1 v2)

prefixOp :: StateT-> PrefixOp -> Value -> StateTransformer Value
prefixOp env PrefixLNot (Bool v) = return $ Bool $ not v
prefixOp env PrefixMinus (Int v) = return $ Int $ (-v)
prefixOp env PrefixMinus (Double v) = return $ Double $ (-v)

unaryAssignOp :: StateT-> UnaryAssignOp -> Value -> StateTransformer Value
unaryAssignOp env PrefixInc (Int v) = return $ Int $ v+1
unaryAssignOp env PostfixInc (Int v) = return $ Int $ v+1
unaryAssignOp env PrefixInc (Double v) = return $ Double $ v+1
unaryAssignOp env PostfixInc (Double v) = return $ Double $ v+1
unaryAssignOp env PrefixDec (Int v) = return $ Int $ v-1
unaryAssignOp env PostfixDec (Int v) = return $ Int $ v-1
unaryAssignOp env PrefixDec (Double v) = return $ Double $ v-1
unaryAssignOp env PostfixDec (Double v) = return $ Double $ v-1

--
-- Environment and auxiliary functions
--

myEquals :: StateT -> [Value] -> [Expression] -> StateTransformer Value
myEquals env l [] = return (Bool True)
myEquals env l (expr:exprs) = do
    evaluedExpr <- evalExpr env expr
    case evaluedExpr of
        (Array l2) -> do
            if (myEqualsArray l l2)
                then myEquals env l exprs
            else return (Bool False)

myEqualsArray :: [Value] -> [Value] -> Bool
myEqualsArray [] [] = True
myEqualsArray x [] = False
myEqualsArray [] y = False
myEqualsArray (x:xs) (y:ys) = (x == y) && (myEqualsArray xs ys)

myConcat :: StateT -> [Value] -> [Expression] -> StateTransformer Value
myConcat env l [] = return (Array l)
myConcat env l (param:params) = do
    evaluedParam <- evalExpr env param
    case evaluedParam of
        (Array l2) -> myConcat env (l ++ l2) params
        v -> myConcat env (l ++ [v]) params

evalArray :: StateT -> [Expression] -> Value -> StateTransformer Value
evalArray env [] (Array l) = return (Array l)
evalArray env (x:xs) (Array l) = do
    evaluedExpr <- evalExpr env x
    evalArray env xs (Array (l++[evaluedExpr]))

myLength :: Int -> [Value] -> Value
myLength x [] = Int x
myLength x (b:bs) = myLength (x+1) bs

evalElementAt :: StateT -> Value -> Value -> StateTransformer Value
evalElementAt env (Array []) (Int n) = return Nil
evalElementAt env (Array (l:ls)) (Int 0) = return l
evalElementAt env (Array (l:ls)) (Int n) = do
    evalElementAt env (Array ls) (Int (n-1))

setVarArray :: Value -> Value -> Value -> Value -> StateTransformer Value
setVarArray (Array x) (Array []) (Int 0) e = return (Array (x ++ [e]))
setVarArray (Array x) (Array []) (Int n) e = setVarArray (Array (x ++ [Empty])) (Array []) (Int (n-1)) e
setVarArray (Array x) (Array (l:ls)) (Int 0) e = return (Array (x ++ [e] ++ ls))
setVarArray (Array x) (Array (l:ls)) (Int n) e = setVarArray (Array (x ++ [l])) (Array ls) (Int (n-1)) e

varDeclList :: StateT -> [VarDecl] -> StateTransformer Value
varDeclList env [] = return Nil
varDeclList env vars = foldl1 (>>) $ map (varDecl env) vars

environment :: StateT
environment = [Map.empty]

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    case scopeLookup s var of
        Nothing -> (GlobalVar, s)
        Just v -> (v, s)

scopeLookup :: StateT -> String -> Maybe Value
scopeLookup [] _ = Nothing
scopeLookup (s:scopes) var =
    case Map.lookup var s of
        Nothing -> scopeLookup scopes var
        Just val -> Just val

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> createLocalVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            createLocalVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, (searchAndUpdateVar var val s))

searchAndUpdateVar :: String -> Value -> StateT -> StateT
searchAndUpdateVar _ _ [] = error $ "Unreachable error"
searchAndUpdateVar var val stt = case (Map.lookup var (head stt)) of
        Nothing -> (head stt):(searchAndUpdateVar var val (tail stt))
        Just v -> (insert var val (head stt)):(tail stt)

createGlobalVar :: String -> Value -> StateTransformer Value
createGlobalVar var val = ST $ \s -> (val, createGlobalVarAux var val s)
 
createGlobalVarAux :: String -> Value -> StateT -> StateT
createGlobalVarAux var val (s:scopes) = 
    if (scopes == []) 
        then (insert var val s):[] 
    else s:(createGlobalVarAux var val scopes)

pushScope :: StateTransformer Value
pushScope = ST $ \s -> (Nil, (Map.empty):s)

popScope :: StateTransformer Value
popScope = ST $ \s -> (Nil, (tail s))

createLocalVar :: String -> Value -> StateTransformer Value
createLocalVar var val = ST $ \s -> (val, (insert var val (head s)):(tail s))

--
-- Types and boilerplate
--

type StateT = [Map String Value]
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, []) = ""
showResult (val, (s:scopes)) =
    show val ++ "\n" ++ show (toList $ union s (Map.empty)) ++ "\n" ++ showResult (val, scopes)

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f [Map.empty]

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
