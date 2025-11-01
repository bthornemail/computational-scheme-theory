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
analyzeScopes expr = evalState (analyzeScopesExpr expr) (Map.empty, 0, [])

-- | Analyze scopes in state monad (tracking current scope and active regions)
-- State: (binding map, depth, active scope regions for nesting)
type ScopeState = (Map.Map BindingId VisibilityRegion, Int, [ScopeRegion])

analyzeScopesExpr :: Expr -> State ScopeState (Map.Map BindingId VisibilityRegion)
analyzeScopesExpr expr = do
  (env, depth, activeRegions) <- get
  let loc = exprLoc expr
  
  case expr of
    Lambda form loc' -> do
      let scopeStart = locPos loc'
      -- Use a larger end position to cover the entire lambda body
      let scopeEnd = scopeStart + 1000 + depth * 100
      
      -- Create scope region for lambda body
      let region = ScopeRegion scopeStart scopeEnd "lambda"
      
      -- Mark parameters as visible in this region AND all outer regions (for nesting)
      let allRegions = region : activeRegions
      modify (\(m, d, rs) -> (foldr (\param -> Map.insertWith 
        (\_ old -> VisibilityRegion $ Set.union (regions old) (Set.fromList allRegions))
        (BindingId param) 
        (VisibilityRegion $ Set.fromList allRegions)) 
        m (lambdaParams form), d + 1, region:rs))
      
      -- Analyze body (with this region as active)
      forM_ (lambdaBody form) analyzeScopesExpr
      modify (\(m, d, rs) -> (m, d - 1, tail rs))
      gets (\(m, _, _) -> m)
      
    Let form loc' -> do
      let scopeStart = locPos loc'
      -- Use larger end position based on depth to ensure nesting overlaps
      let scopeEnd = scopeStart + 1000 + depth * 100
      let region = ScopeRegion scopeStart scopeEnd "let"
      
      -- Mark bindings as visible in THIS region AND all outer regions (for nesting)
      -- This ensures nested lets create overlapping visibility regions
      let allRegions = region : activeRegions
      modify (\(m, d, rs) -> (foldr (\(name, _) -> Map.insertWith
        (\_ old -> VisibilityRegion $ Set.union (regions old) (Set.fromList allRegions))
        (BindingId name)
        (VisibilityRegion $ Set.fromList allRegions))
        m (letBindings form), d + 1, region:rs))
      
      -- Analyze bindings and body (with this region as active)
      forM_ (map snd (letBindings form)) analyzeScopesExpr
      forM_ (letBody form) analyzeScopesExpr
      modify (\(m, d, rs) -> (m, d - 1, tail rs))
      gets (\(m, _, _) -> m)
      
    LetRec form loc' -> do
      let scopeStart = locPos loc'
      let scopeEnd = scopeStart + 1000 + depth * 100
      let region = ScopeRegion scopeStart scopeEnd "letrec"
      
      -- Same as Let: bindings visible in this region and outer regions
      let allRegions = region : activeRegions
      modify (\(m, d, rs) -> (foldr (\(name, _) -> Map.insertWith
        (\_ old -> VisibilityRegion $ Set.union (regions old) (Set.fromList allRegions))
        (BindingId name)
        (VisibilityRegion $ Set.fromList allRegions))
        m (letBindings form), d + 1, region:rs))
      
      forM_ (map snd (letBindings form)) analyzeScopesExpr
      forM_ (letBody form) analyzeScopesExpr
      modify (\(m, d, rs) -> (m, d - 1, tail rs))
      gets (\(m, _, _) -> m)
      
    Define (DefineVar name _) loc' -> do
      let scopeStart = locPos loc'
      let scopeEnd = scopeStart + 10000  -- Top-level define visible globally
      let region = ScopeRegion scopeStart scopeEnd "define"
      let allRegions = region : activeRegions
      modify (\(m, d, rs) -> (Map.insertWith
        (\_ old -> VisibilityRegion $ Set.union (regions old) (Set.fromList allRegions))
        (BindingId name) 
        (VisibilityRegion $ Set.fromList allRegions) m, d, rs))
      gets (\(m, _, _) -> m)
      
    Define (DefineFun name params body) loc' -> do
      let scopeStart = locPos loc'
      let scopeEnd = scopeStart + 10000  -- Function definition visible globally
      let region = ScopeRegion scopeStart scopeEnd "define-fun"
      let allRegions = region : activeRegions
      modify (\(m, d, rs) -> (Map.insertWith
        (\_ old -> VisibilityRegion $ Set.union (regions old) (Set.fromList allRegions))
        (BindingId name)
        (VisibilityRegion $ Set.fromList allRegions) $
        foldr (\param -> Map.insertWith
          (\_ old -> VisibilityRegion $ Set.union (regions old) (Set.fromList allRegions))
          (BindingId param)
          (VisibilityRegion $ Set.fromList allRegions))
          m params, d, region:rs))
      forM_ body analyzeScopesExpr
      modify (\(m, d, rs) -> (m, d, tail rs))
      gets (\(m, _, _) -> m)
      
    _ -> gets (\(m, _, _) -> m)

