{-# LANGUAGE OverloadedStrings #-}

-- | Binding Algebra Extractor (Algorithm 1)
--
-- This module implements Algorithm 1: extracting the R_Scheme rig from
-- R5RS Scheme source code.
--
-- Steps:
--   1. Parse S-expression → AST
--   2. Identify binding forms (lambda, let, define)
--   3. Apply α-conversion (hygienic renaming)
--   4. Build rig: Collect all unique bindings into R_Scheme
--   5. Record scopes: Track where each binding is visible
module ComputationalScheme.Algorithm1.BindingExtractor where

import ComputationalScheme.Types (RScheme(..), BindingId(..))
import ComputationalScheme.Rig
import ComputationalScheme.Algorithm1.AST
import ComputationalScheme.Algorithm1.Parser
import ComputationalScheme.Algorithm1.AlphaConversion
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Control.Monad.State

-- | Extract binding algebra from Scheme source code
-- This is the main entry point for Algorithm 1.
--
-- @
-- extractBindingAlgebra "(lambda (x) x)" 
--   => RScheme { bindings = fromList [BindingId "α0"] }
-- @
extractBindingAlgebra :: Text -> Either String RScheme
extractBindingAlgebra source = do
  -- Step 1: Parse S-expression → AST
  exprs <- case parseProgram source of
    Left err -> Left $ "Parse error: " ++ show err
    Right es -> Right es
  
  -- Step 2-3: Apply α-conversion to all expressions
  let convertedExprs = map alphaConvert exprs
  
  -- Step 4-5: Extract bindings and build R_Scheme rig
  let bindings = foldMap extractBindingsFromExpr convertedExprs
  
  return $ RScheme bindings

-- | Extract all bindings from an expression
extractBindingsFromExpr :: Expr -> Set.Set BindingId
extractBindingsFromExpr expr = case expr of
  Lambda form _ -> 
    let paramBindings = Set.fromList $ map BindingId (lambdaParams form)
        bodyBindings = foldMap extractBindingsFromExpr (lambdaBody form)
    in Set.union paramBindings bodyBindings
    
  Let form _ ->
    let bindingIds = Set.fromList $ map (BindingId . fst) (letBindings form)
        valueBindings = foldMap (extractBindingsFromExpr . snd) (letBindings form)
        bodyBindings = foldMap extractBindingsFromExpr (letBody form)
    in Set.unions [bindingIds, valueBindings, bodyBindings]
    
  LetRec form _ ->
    let bindingIds = Set.fromList $ map (BindingId . fst) (letBindings form)
        valueBindings = foldMap (extractBindingsFromExpr . snd) (letBindings form)
        bodyBindings = foldMap extractBindingsFromExpr (letBody form)
    in Set.unions [bindingIds, valueBindings, bodyBindings]
    
  Define (DefineVar name _) _ ->
    Set.singleton $ BindingId name
    
  Define (DefineFun name params body) _ ->
    let paramBindings = Set.fromList $ map BindingId params
        bodyBindings = foldMap extractBindingsFromExpr body
    in Set.insert (BindingId name) $ Set.union paramBindings bodyBindings
    
  If form _ ->
    Set.unions
      [ extractBindingsFromExpr (ifTest form)
      , extractBindingsFromExpr (ifThen form)
      , extractBindingsFromExpr (ifElse form)
      ]
      
  Cond clauses _ ->
    foldMap extractBindingsFromClause clauses
    
  Application fn args _ ->
    Set.unions $ extractBindingsFromExpr fn : map extractBindingsFromExpr args
    
  Begin exprs _ ->
    foldMap extractBindingsFromExpr exprs
    
  CallCC e _ ->
    extractBindingsFromExpr e
    
  _ -> Set.empty  -- Constants and variables don't create bindings

-- | Extract bindings from a cond clause
extractBindingsFromClause :: CondClause -> Set.Set BindingId
extractBindingsFromClause (CondClause test body) =
  let testBindings = maybe Set.empty extractBindingsFromExpr test
      bodyBindings = foldMap extractBindingsFromExpr body
  in Set.union testBindings bodyBindings

