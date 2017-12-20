module Transformers where

import Control.Monad.Identity
import Data.Maybe (fromJust)
import qualified Data.Map as Map

-- ###############################
type Name = String
data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp -- lambda abstraction
         | App Exp Exp -- function application

instance Show Exp where
  show (Lit i) = show i
  show (Var n) = n
  show (Plus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Abs n e) = "(\\" ++ n ++ " -> " ++ show e ++ ")"
  show (App e1 e2) = show e1 ++ " " ++ show e2

data Value = IntVal Integer
           | FunVal Env Name Exp

instance Show Value where
  show (IntVal i) = show i
  show (FunVal _ n e) = "Fun: \\" ++ n ++ " -> " ++ show e

type Env = Map.Map Name Value

-- ###############################
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ Map.lookup n env
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal $ i1 + i2
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                             FunVal env' n body -> eval0 (Map.insert n val2 env') body

var = "x"
-- function (plus 1): \x -> x + 1
plusOne = Abs var (Var var `Plus` Lit 1)
-- 12 + ((\x -> x) (4 + 2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- ###############################
-- convert eval0 to a monadic style

type Eval1 a = Identity a
runEval1  :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do
  IntVal i1 <- eval1 env e1
  IntVal i2 <- eval1 env e2
  return $ IntVal $ i1 + i2
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body -> eval1 (Map.insert n val2 env') body
