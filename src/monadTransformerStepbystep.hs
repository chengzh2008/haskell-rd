module Transformers where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
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
exampleExp = Lit 12 `Plus` (App (Abs var (Var var)) (Lit 4 `Plus` Lit 2))
exampleWithTypeMisMatch1 = Lit 12 `Plus` plusOne
exampleWithTypeMisMatch2 = Plus (Lit 1) (Abs var (Var var))
exampleWithKeyNotFound = Var var
exampleWithWrongApp = App (Lit 2) (Lit 1)

-- ###############################
-- convert eval0 to a monadic style

type Eval1 a = Identity a
runEval1  :: Eval1 a -> a
runEval1 = runIdentity


eval1 ::Monad m => Env -> Exp -> m Value
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

-- ###############################
-- add error handling to eval0

type Eval2 a = ExceptT String Identity a
runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runExceptT

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = return $ fromJust $ Map.lookup n env
eval2a env (Plus e1 e2) = do
  IntVal i1 <- eval2a env e1
  IntVal i2 <- eval2a env e2
  return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do
  val1 <- eval2a env e1
  val2 <- eval2a env e2
  case val1 of
    FunVal env' n body -> eval2a (Map.insert n val2 env') body

-- eval2a Map.empty typeError will have pattern match failure
-- need to add error handling in eval2a

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = case Map.lookup n env of
  Nothing -> throwError $ "Key not found: " ++ n
  Just x -> return x
eval2b env (Plus e1 e2) = do
  e1' <- eval2b env e1
  e2' <- eval2b env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError $ "type Error: " ++ show e1 ++ " and " ++ show e2 ++ " can not be added together."
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do
  val1 <- eval2b env e1
  val2 <- eval2b env e2
  case val1 of
    FunVal env' n body -> eval2b (Map.insert n val2 env') body
    _ -> throwError $ "Application Error: " ++ show e1 ++ " is not a function."


-- ###################################
-- hiding the env variable from every function
type Eval3 a = ReaderT Env (ExceptT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runExceptT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError $ "Key not found: " ++ n
    Just x -> return x
eval3 (Plus e1 e2) = do
  e1' <- eval3 e1
  e2' <- eval3 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError $ "type Error: " ++ show e1 ++ " and " ++ show e2 ++ " can not be added together."
eval3 (Abs n e) = do
  env <- ask
  return $ FunVal env n e
eval3 (App e1 e2) = do
  val1 <- eval3 e1
  val2 <- eval3 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body) -- not quite understand here "local and const"
    _ -> throwError $ "Application Error: " ++ show e1 ++ " is not a function."

-- ###################################
-- add a profiling capability to our function

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a
runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT (runExceptT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do
  tick
  return $ IntVal i
eval4 (Var n) = do
  tick
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError $ "Key not found: " ++ n
    Just x -> return x
eval4 (Plus e1 e2) = do
  tick
  e1' <- eval4 e1
  e2' <- eval4 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError $ "type Error: " ++ show e1 ++ " and " ++ show e2 ++ " can not be added together."
eval4 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval4 (App e1 e2) = do
  tick
  val1 <- eval4 e1
  val2 <- eval4 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body) -- not quite understand here "local and const"
    _ -> throwError $ "Application Error: " ++ show e1 ++ " is not a function."

-- swap the StateT with ExceptT

type Eval4' a = ReaderT Env (StateT Integer (ExceptT String Identity)) a
runEval4' :: Env -> Integer -> Eval4' a -> Either String (a, Integer)
runEval4' env st ev = runIdentity (runExceptT (runStateT (runReaderT ev env) st))

-- ###############################
-- add a logging

type Eval5 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a
runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do
  tick
  tell ["evaluation: " ++ show i]
  return $ IntVal i
eval5 (Var n) = do
  tick
  tell ["evaluation: " ++ n]
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError $ "Key not found: " ++ n
    Just x -> return x
eval5 (Plus e1 e2) = do
  tick
  tell ["evaluation: " ++ "exp " ++ show e1 ++ " and exp " ++ show e2]
  e1' <- eval5 e1
  e2' <- eval5 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError $ "type Error: " ++ show e1 ++ " and " ++ show e2 ++ " can not be added together."
eval5 (Abs n e) = do
  tick
  tell ["evaluation function" ++ show (Abs n e)]
  env <- ask
  return $ FunVal env n e
eval5 (App e1 e2) = do
  tick
  tell ["evaluation application: " ++ show e1 ++ " " ++ show e2]
  val1 <- eval5 e1
  val2 <- eval5 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval5 body) -- not quite understand here "local and const"
    _ -> throwError $ "Application Error: " ++ show e1 ++ " is not a function."

-- ######################################
-- how about IO, replace Identity with IO.

type Eval6 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) a
runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st


eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do
  tick
  liftIO $ print (Lit i)
  tell ["evaluation: " ++ show i]
  return $ IntVal i
eval6 (Var n) = do
  tick
  liftIO $ print (Var n)
  tell ["evaluation: " ++ n]
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError $ "Key not found: " ++ n
    Just x -> return x
eval6 a@(Plus e1 e2) = do
  tick
  liftIO $ print a
  tell ["evaluation: " ++ "exp " ++ show e1 ++ " and exp " ++ show e2]
  e1' <- eval6 e1
  e2' <- eval6 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError $ "type Error: " ++ show e1 ++ " and " ++ show e2 ++ " can not be added together."
eval6 a@(Abs n e) = do
  tick
  liftIO $ print a
  tell ["evaluation function" ++ show (Abs n e)]
  env <- ask
  return $ FunVal env n e
eval6 a@(App e1 e2) = do
  tick
  liftIO $ print a
  tell ["evaluation application: " ++ show e1 ++ " " ++ show e2]
  val1 <- eval6 e1
  val2 <- eval6 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body) -- not quite understand here "local and const"
    _ -> throwError $ "Application Error: " ++ show e1 ++ " is not a function."
