{-# LANGUAGE OverloadedStrings #-}

-- | α-conversion (hygienic renaming) for Scheme bindings
--
-- This module implements hygienic renaming to ensure all bindings have
-- unique identifiers, handling shadowing and maintaining binding structure.
module ComputationalScheme.Algorithm1.AlphaConversion where

import ComputationalScheme.Algorithm1.AST
import ComputationalScheme.Types (BindingId(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (forM)
import Data.List (elemIndex)

-- | Environment mapping original names to unique binding IDs
type Env = Map.Map Text BindingId

-- | Counter state for generating unique binding IDs
type AlphaState = (Int, Env)

-- | Perform α-conversion on an expression
-- Returns the expression with all bindings renamed to unique IDs
alphaConvert :: Expr -> Expr
alphaConvert expr = evalState (alphaConvertExpr expr) (0, Map.empty)

-- | Convert an expression in state monad
alphaConvertExpr :: Expr -> State AlphaState Expr
alphaConvertExpr expr = case expr of
  Const c loc            -> return $ Const c loc
  Var name loc           -> do
    env <- gets snd
    case Map.lookup name env of
      Just bid -> return $ Var (unBindingId bid) loc
      Nothing  -> return $ Var name loc  -- Free variable
      
  Lambda form loc        -> do
    (counter, env) <- get
    let (newIds, newCounter) = generateIds (lambdaParams form) counter
    let newEnv = foldr (uncurry Map.insert) env (zip (lambdaParams form) newIds)
    put (newCounter, newEnv)
    newBody <- mapM alphaConvertExpr (lambdaBody form)
    put (newCounter, env)  -- Restore outer environment
    return $ Lambda (LambdaForm (map unBindingId newIds) newBody) loc
    
  Let form loc           -> alphaConvertLet Let form loc
  LetRec form loc        -> alphaConvertLet LetRec form loc
  
  Define (DefineVar name val) loc -> do
    val' <- alphaConvertExpr val
    return $ Define (DefineVar name val') loc
    
  Define (DefineFun name params body) loc -> do
    (counter, env) <- get
    let (newIds, newCounter) = generateIds params counter
    let newEnv = Map.insert name (head newIds) $ foldr (uncurry Map.insert) env (zip params newIds)
    put (newCounter, newEnv)
    newBody <- mapM alphaConvertExpr body
    put (newCounter, env)  -- Restore outer environment
    return $ Define (DefineFun name (map unBindingId newIds) newBody) loc
    
  If form loc            -> do
    test' <- alphaConvertExpr (ifTest form)
    then' <- alphaConvertExpr (ifThen form)
    else' <- alphaConvertExpr (ifElse form)
    return $ If (IfForm test' then' else') loc
    
  Cond clauses loc       -> do
    newClauses <- mapM alphaConvertClause clauses
    return $ Cond newClauses loc
    
  Application fn args loc -> do
    fn' <- alphaConvertExpr fn
    args' <- mapM alphaConvertExpr args
    return $ Application fn' args' loc
    
  Begin exprs loc        -> do
    exprs' <- mapM alphaConvertExpr exprs
    return $ Begin exprs' loc
    
  CallCC e loc           -> do
    e' <- alphaConvertExpr e
    return $ CallCC e' loc

-- | Helper for Let and LetRec forms
alphaConvertLet :: (LetForm -> SourceLoc -> Expr) -> LetForm -> SourceLoc -> State AlphaState Expr
alphaConvertLet constructor form loc = do
  (counter, env) <- get
  let paramNames = map fst (letBindings form)
  let (newIds, newCounter) = generateIds paramNames counter
  let newEnv = foldr (uncurry Map.insert) env (zip paramNames newIds)
  put (newCounter, newEnv)
  
  -- Convert binding values in new environment
  newBindings <- forM (letBindings form) $ \(name, val) -> do
    val' <- alphaConvertExpr val
    let idx = findIndex name paramNames
    return (unBindingId (newIds !! idx), val')
  
  -- Convert body
  newBody <- mapM alphaConvertExpr (letBody form)
  put (newCounter, env)  -- Restore outer environment
  
  return $ constructor (LetForm newBindings newBody) loc
  where
    findIndex name names = case elemIndex name names of
      Just i -> i
      Nothing -> error "internal error: name not found in alpha conversion"

-- | Convert a cond clause
alphaConvertClause :: CondClause -> State AlphaState CondClause
alphaConvertClause (CondClause test body) = do
  test' <- maybe (return Nothing) (fmap Just . alphaConvertExpr) test
  body' <- mapM alphaConvertExpr body
  return $ CondClause test' body'

-- | Generate unique binding IDs for a list of parameter names
generateIds :: [Text] -> Int -> ([BindingId], Int)
generateIds names counter = (newIds, counter + length names)
  where
    newIds = [ BindingId $ "α" <> T.pack (show (counter + i)) | i <- [0..length names - 1] ]

