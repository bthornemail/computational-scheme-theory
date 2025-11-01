{-# LANGUAGE OverloadedStrings #-}

-- | Scope analysis: Compute visibility regions D(f)
--
-- For each binding f, D(f) is the set of all program positions where
-- f is visible (in scope). This forms the basis for the Zariski topology.
module ComputationalScheme.Algorithm2.Scope where

import ComputationalScheme.Types
import ComputationalScheme.Algorithm1.AST
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (forM_)

-- | Binding with its scope information
data BindingScope = BindingScope
  { bindingId :: BindingId
  , originalName :: Text
  , bindingType :: BindingType
  , visibilityRegions :: VisibilityRegion
  }
  deriving (Eq, Show)

-- | Type of binding (lambda parameter, let binding, define, etc.)
data BindingType
  = LambdaParam
  | LetBinding
  | LetRecBinding
  | DefineBinding
  | TopLevelDefine
  deriving (Eq, Show)

-- | Map from binding ID to its visibility region
type BindingScopeMap = Map.Map BindingId VisibilityRegion

-- | Analyze expression to extract scope regions for all bindings
-- Returns a map from BindingId to VisibilityRegion (D(f) for each binding)
analyzeScopes :: Expr -> BindingScopeMap
analyzeScopes expr = evalState (analyzeScopesExpr expr) (Map.empty, 0)

-- | Analyze scopes in state monad (tracking current scope)
type ScopeState = (Map.Map BindingId VisibilityRegion, Int)

analyzeScopesExpr :: Expr -> State ScopeState (Map.Map BindingId VisibilityRegion)
analyzeScopesExpr expr = do
  (env, depth) <- get
  let loc = exprLoc expr
  
  case expr of
    Lambda form loc' -> do
      let scopeStart = locPos loc'
      let scopeEnd = locPos loc'  -- Simplified - would compute actual end
      
      -- Create scope region for lambda body
      let region = ScopeRegion scopeStart scopeEnd "lambda"
      
      -- Mark parameters as visible in this region
      modify (\(m, d) -> (foldr (\param -> Map.insertWith 
        (\_ old -> VisibilityRegion $ Set.insert region (regions old))
        (BindingId param) 
        (VisibilityRegion $ Set.singleton region)) 
        m (lambdaParams form), d + 1))
      
      -- Analyze body
      forM_ (lambdaBody form) analyzeScopesExpr
      modify (\(m, d) -> (m, d - 1))
      gets fst
      
    Let form loc' -> do
      let region = ScopeRegion (locPos loc') (locPos loc' + 100) "let"
      
      -- Mark bindings as visible
      modify (\(m, d) -> (foldr (\(name, _) -> Map.insertWith
        (\_ old -> VisibilityRegion $ Set.insert region (regions old))
        (BindingId name)
        (VisibilityRegion $ Set.singleton region))
        m (letBindings form), d + 1))
      
      forM_ (map snd (letBindings form)) analyzeScopesExpr
      forM_ (letBody form) analyzeScopesExpr
      modify (\(m, d) -> (m, d - 1))
      gets fst
      
    LetRec form loc' -> do
      let region = ScopeRegion (locPos loc') (locPos loc' + 100) "letrec"
      
      modify (\(m, d) -> (foldr (\(name, _) -> Map.insertWith
        (\_ old -> VisibilityRegion $ Set.insert region (regions old))
        (BindingId name)
        (VisibilityRegion $ Set.singleton region))
        m (letBindings form), d + 1))
      
      forM_ (map snd (letBindings form)) analyzeScopesExpr
      forM_ (letBody form) analyzeScopesExpr
      modify (\(m, d) -> (m, d - 1))
      gets fst
      
    Define (DefineVar name _) loc' -> do
      let region = ScopeRegion (locPos loc') (locPos loc' + 1000) "define"
      modify (\(m, d) -> (Map.insert (BindingId name) (VisibilityRegion $ Set.singleton region) m, d))
      gets fst
      
    Define (DefineFun name params body) loc' -> do
      let region = ScopeRegion (locPos loc') (locPos loc' + 1000) "define-fun"
      modify (\(m, d) -> (Map.insert (BindingId name) (VisibilityRegion $ Set.singleton region) $
        foldr (\param -> Map.insertWith
          (\_ old -> VisibilityRegion $ Set.insert region (regions old))
          (BindingId param)
          (VisibilityRegion $ Set.singleton region))
          m params, d))
      forM_ body analyzeScopesExpr
      gets fst
      
    _ -> gets fst

