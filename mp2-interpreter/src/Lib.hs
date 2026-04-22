module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import Data.Maybe


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = fromMaybe (ExnVal "No match in env") (H.lookup s env)

--- ### Arithmetic

eval (IntOpExp "/" e1 e2) env
    | eval e2 env == IntVal 0 = ExnVal "Division by 0"
    | otherwise = liftIntOp div (eval e1 env) (eval e2 env)
eval (IntOpExp op e1 e2) env = liftIntOp (fromMaybe (+) (H.lookup op intOps)) (eval e1 env) (eval e2 env)

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = liftBoolOp (fromMaybe (&&) (H.lookup op boolOps)) (eval e1 env) (eval e2 env)

eval (CompOpExp op e1 e2) env = liftCompOp (fromMaybe (>) (H.lookup op compOps)) (eval e1 env) (eval e2 env)

--- ### If Expressions

eval (IfExp e1 e2 e3) env
    | eval e1 env == BoolVal True = eval e2 env
    | eval e1 env == BoolVal False = eval e3 env
eval (IfExp _ _ _) _ = ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

-- data Exp = IntExp Int
--          | FunExp [String (params)] Exp (body)
--          | AppExp FunExp [Exp]

eval (FunExp params body) env = CloVal params body env

-- CloVal [String] FunExp Env
-- FunExp [String] Exp
-- MEOW
eval (AppExp fun args) env = 
    case eval fun env of
        CloVal params body clenv ->
            let insertparams [] [] envv = envv
                insertparams (x:xs) (y:ys) envv = insertparams xs ys (insert x (eval y env) envv)
            in eval body (insertparams params args clenv)
        _ -> ExnVal "Apply to non-closure"


--- ### Let Expressions

-- LetExp [(String,Exp)] Exp
eval (LetExp [] body) env = eval body env
eval (LetExp (x:xs) body) env = eval (LetExp xs body) (insert k v env)
    where k = fst x
          v = eval (snd x) env

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

-- type Env  = H.HashMap String Val
-- type PEnv = H.HashMap String Stmt

-- type Result = (String, PEnv, Env)
-- SetStmt String Exp

exec (SetStmt var e) penv env = ("", penv, (insert var (eval e env) env))

--- ### Sequencing
-- SeqStmt [Stmt]

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = (s1 ++ s2, p2, e2)
    where (s2, p2, e2) = exec (SeqStmt xs) p1 e1
          (s1, p1, e1) = exec x penv env

--- ### If Statements

exec (IfStmt e s1 s2) penv env
    | eval e env == BoolVal True = exec s1 penv env
    | eval e env == BoolVal False = exec s2 penv env
exec (IfStmt _ _ _) penv env             = ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

--           | ProcedureStmt String [String] Stmt
--           | CallStmt String [Exp]

exec p@(ProcedureStmt name args body) penv env = ("", insert name (ProcedureStmt name args body) penv, env)

-- find function body (lookup in penv)
-- add parameter values to env (need to get names of params, have values in args)

exec (CallStmt name args) penv env = 
    case H.lookup name penv of
        Just (ProcedureStmt _ ps body) -> 
            let argVals = map (\a -> eval a env) args
                env2 = bindParams ps argVals env
            in exec body penv env2
        Nothing -> ("Procedure " ++ name ++ " undefined", penv, env)

bindParams [] [] env = env
bindParams (p:ps) (v:vs) env =
    bindParams ps vs (insert p v env)
bindParams _ _ _ = error "Argument/parameter mismatch"
        
