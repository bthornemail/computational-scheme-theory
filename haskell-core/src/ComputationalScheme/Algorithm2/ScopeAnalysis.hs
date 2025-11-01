{-# LANGUAGE OverloadedStrings #-}

-- | Enhanced Scope Analysis with Scope Tree and Usage Pattern Tracking
--
-- This module implements the alternative approach:
-- 1. Builds explicit scope tree (parent-child relationships)
-- 2. Tracks usage patterns (where bindings are referenced)
-- 3. Assigns distinct regions based on usage contexts
module ComputationalScheme.Algorithm2.ScopeAnalysis where

import ComputationalScheme.Types
import ComputationalScheme.Algorithm1.AST
import ComputationalScheme.Algorithm1.AST (SourceLoc(..), exprLoc, locPos)
import ComputationalScheme.Algorithm2.ScopeTree (getDescendants)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (forM_)
import Debug.Trace (trace)

-- | Enhanced state for scope analysis
data EnhancedScopeState = EnhancedScopeState
  { bindingMap :: Map.Map BindingId EnhancedVisibilityRegion
  , scopeTree :: ScopeTree
  , currentScope :: Maybe ScopeId
  , depth :: Int
  , usagePatterns :: Map.Map BindingId UsagePattern
  }

-- | Initial scope tree (empty)
emptyScopeTree :: ScopeTree
emptyScopeTree = ScopeTree
  { nodes = Map.empty
  , root = ScopeId 0
  , nextId = 1
  }

-- | Create a new scope node
createScopeNode :: ScopeTree -> Maybe ScopeId -> ScopeType -> Int -> (ScopeTree, ScopeId)
createScopeNode tree parentId scopeType depth =
  let newId = ScopeId (nextId tree)
      newNode = ScopeTreeNode
        { nodeId = newId
        , parent = parentId
        , children = []
        , scopeBindings = Set.empty
        , scopeType = scopeType
        , scopeDepth = depth
        }
      -- Update parent's children list
      updatedNodes = Map.insert newId newNode (nodes tree)
      finalNodes = case parentId of
        Just pid -> 
          case Map.lookup pid updatedNodes of
            Just parentNode -> 
              Map.insert pid (parentNode { children = newId : children parentNode }) updatedNodes
            Nothing -> updatedNodes
        Nothing -> updatedNodes
      newTree = tree { nodes = finalNodes, nextId = nextId tree + 1 }
  in (newTree, newId)

-- | Analyze expression with enhanced scope tracking
analyzeScopesEnhanced :: Expr -> (Map.Map BindingId EnhancedVisibilityRegion, ScopeTree)
analyzeScopesEnhanced expr = 
  let initialState = EnhancedScopeState
        { bindingMap = Map.empty
        , scopeTree = emptyScopeTree
        , currentScope = Nothing
        , depth = 0
        , usagePatterns = Map.empty
        }
      (_, finalState) = runState (analyzeScopesExprEnhanced expr) initialState
      finalTree = scopeTree finalState
      -- Compute final visibility: bindings visible in definition scope + all descendant scopes
      finalBindings = computeFinalVisibility finalTree (bindingMap finalState)
  in (finalBindings, finalTree)

-- | Analyze expression in enhanced state monad
analyzeScopesExprEnhanced :: Expr -> State EnhancedScopeState ()
analyzeScopesExprEnhanced expr = do
  state <- get
  let loc = exprLoc expr
  
  case expr of
    Lambda form loc' -> do
      -- Create new scope node for lambda
      (tree, newScopeId) <- gets $ \s -> 
        createScopeNode (scopeTree s) (currentScope s) LambdaScope (depth s)
      modify $ \s -> s 
        { scopeTree = tree
        , currentScope = Just newScopeId
        , depth = depth s + 1
        }
      
      -- Register parameters as bindings in this scope
      let params = lambdaParams form
      forM_ params $ \param -> do
        let bindingId = BindingId param
        -- Create initial usage pattern (will be updated when referenced)
        let pattern = UsagePattern
              { bindingId = bindingId
              , usageLocations = []
              , usageContexts = []
              }
        modify $ \s -> s
          { bindingMap = Map.insert bindingId (createEnhancedRegion loc' newScopeId pattern) (bindingMap s)
          , usagePatterns = Map.insert bindingId pattern (usagePatterns s)
          , scopeTree = addBindingToScope (scopeTree s) newScopeId bindingId
          }
      
      -- Analyze body
      forM_ (lambdaBody form) analyzeScopesExprEnhanced
      
      -- Pop scope
      modify $ \s -> s
        { currentScope = case currentScope s of
            Just (ScopeId _) -> parentOfScope (scopeTree s) (currentScope s)
            Nothing -> Nothing
        , depth = depth s - 1
        }
    
    Let form loc' -> do
      -- Create new scope node for let
      (tree, newScopeId) <- gets $ \s -> 
        createScopeNode (scopeTree s) (currentScope s) LetScope (depth s)
      modify $ \s -> s
        { scopeTree = tree
        , currentScope = Just newScopeId
        , depth = depth s + 1
        }
      
      -- Register let bindings
      forM_ (letBindings form) $ \(name, _) -> do
        let bindingId = BindingId name
        let pattern = UsagePattern
              { bindingId = bindingId
              , usageLocations = []
              , usageContexts = []
              }
        modify $ \s -> s
          { bindingMap = Map.insert bindingId (createEnhancedRegion loc' newScopeId pattern) (bindingMap s)
          , usagePatterns = Map.insert bindingId pattern (usagePatterns s)
          , scopeTree = addBindingToScope (scopeTree s) newScopeId bindingId
          }
      
      -- Analyze bindings and body
      forM_ (map snd (letBindings form)) analyzeScopesExprEnhanced
      forM_ (letBody form) analyzeScopesExprEnhanced
      
      -- Pop scope
      modify $ \s -> s
        { currentScope = parentOfScope (scopeTree s) (currentScope s)
        , depth = depth s - 1
        }
    
    LetRec form loc' -> do
      -- Similar to Let but with recursive bindings
      (tree, newScopeId) <- gets $ \s -> 
        createScopeNode (scopeTree s) (currentScope s) LetRecScope (depth s)
      modify $ \s -> s
        { scopeTree = tree
        , currentScope = Just newScopeId
        , depth = depth s + 1
        }
      
      forM_ (letBindings form) $ \(name, _) -> do
        let bindingId = BindingId name
        let pattern = UsagePattern
              { bindingId = bindingId
              , usageLocations = []
              , usageContexts = [RecursiveCall]  -- Mark as recursive
              }
        modify $ \s -> s
          { bindingMap = Map.insert bindingId (createEnhancedRegion loc' newScopeId pattern) (bindingMap s)
          , usagePatterns = Map.insert bindingId pattern (usagePatterns s)
          , scopeTree = addBindingToScope (scopeTree s) newScopeId bindingId
          }
      
      forM_ (map snd (letBindings form)) analyzeScopesExprEnhanced
      forM_ (letBody form) analyzeScopesExprEnhanced
      
      modify $ \s -> s
        { currentScope = parentOfScope (scopeTree s) (currentScope s)
        , depth = depth s - 1
        }
    
    Define (DefineFun name params body) loc' -> do
      -- Top-level function definition
      (tree, newScopeId) <- gets $ \s -> 
        createScopeNode (scopeTree s) Nothing TopLevelScope 0
      modify $ \s -> s
        { scopeTree = tree
        , currentScope = Just newScopeId
        }
      
      -- Register function name and parameters
      let funcBindingId = BindingId name
      let funcPattern = UsagePattern
            { bindingId = funcBindingId
            , usageLocations = [locPos loc']
            , usageContexts = [NormalContext]
            }
      modify $ \s -> s
        { bindingMap = Map.insert funcBindingId (createEnhancedRegion loc' newScopeId funcPattern) (bindingMap s)
        , usagePatterns = Map.insert funcBindingId funcPattern (usagePatterns s)
        , scopeTree = addBindingToScope (scopeTree s) newScopeId funcBindingId
        }
      
      -- Register parameters with distinct sub-scopes based on usage
      -- Each parameter gets its own scope to enable distinct regions
      forM_ params $ \param -> do
        let bindingId = BindingId param
        let pattern = UsagePattern
              { bindingId = bindingId
              , usageLocations = []
              , usageContexts = []
              }
        -- Create a sub-scope for each parameter to track distinct usage
        (subTree, paramScopeId) <- gets $ \s -> 
          createScopeNode (scopeTree s) (Just newScopeId) LambdaScope 1
        modify $ \s -> s
          { bindingMap = Map.insert bindingId (createEnhancedRegion loc' paramScopeId pattern) (bindingMap s)
          , usagePatterns = Map.insert bindingId pattern (usagePatterns s)
          , scopeTree = addBindingToScope subTree paramScopeId bindingId
          }
      
      -- Analyze body to track parameter usage
      forM_ body analyzeScopesExprEnhanced
      
      modify $ \s -> s { currentScope = Nothing }
    
    Var name loc' -> do
      -- Track usage of variable (but don't modify visibility here)
      -- Visibility will be computed after scope tree is complete based on lexical scoping
      let bindingId = BindingId name
      state <- get
      case Map.lookup bindingId (bindingMap state) of
        Just region -> do
          -- Update usage pattern only
          let pattern = case Map.lookup bindingId (usagePatterns state) of
                Just p -> p { 
                  usageLocations = locPos loc' : usageLocations p,
                  usageContexts = getCurrentContext state : usageContexts p
                }
                Nothing -> UsagePattern bindingId [locPos loc'] [getCurrentContext state]
          
          -- Update usage pattern but NOT scope visibility
          -- Final visibility will be computed from scope tree (definition scope + descendants)
          modify $ \s -> s
            { bindingMap = Map.insert bindingId (region { usagePattern = pattern }) (bindingMap s)
            , usagePatterns = Map.insert bindingId pattern (usagePatterns s)
            }
        Nothing -> return ()  -- Undefined variable, ignore
    
    If form loc' -> do
      -- Analyze test
      analyzeScopesExprEnhanced (ifTest form)
      
      -- Create scope for then branch
      (tree1, thenScopeId) <- gets $ \s -> 
        createScopeNode (scopeTree s) (currentScope s) IfScope (depth s + 1)
      modify $ \s -> s { scopeTree = tree1, depth = depth s + 1 }
      analyzeScopesExprEnhanced (ifThen form)
      modify $ \s -> s { depth = depth s - 1 }
      
      -- Create scope for else branch
      (tree2, elseScopeId) <- gets $ \s -> 
        createScopeNode (scopeTree s) (currentScope s) IfScope (depth s + 1)
      modify $ \s -> s { scopeTree = tree2, depth = depth s + 1 }
      analyzeScopesExprEnhanced (ifElse form)
      modify $ \s -> s { depth = depth s - 1 }
    
    Application func args loc' -> do
      -- Analyze function and arguments
      analyzeScopesExprEnhanced func
      forM_ args analyzeScopesExprEnhanced
    
    Begin exprs loc' -> do
      -- Analyze all expressions in sequence
      forM_ exprs analyzeScopesExprEnhanced
    
    CallCC expr loc' -> do
      -- Analyze continuation expression
      analyzeScopesExprEnhanced expr
    
    Define (DefineVar name val) loc' -> do
      -- Top-level variable definition
      (tree, newScopeId) <- gets $ \s -> 
        createScopeNode (scopeTree s) Nothing TopLevelScope 0
      let bindingId = BindingId name
      let pattern = UsagePattern bindingId [locPos loc'] [NormalContext]
      modify $ \s -> s
        { bindingMap = Map.insert bindingId (createEnhancedRegion loc' newScopeId pattern) (bindingMap s)
        , usagePatterns = Map.insert bindingId pattern (usagePatterns s)
        , scopeTree = addBindingToScope tree newScopeId bindingId
        }
      analyzeScopesExprEnhanced val
    
    Const _ _ -> return ()  -- Constants don't create scopes
    
    Cond _ _ -> return ()  -- TODO: Handle cond

-- | Create enhanced visibility region
createEnhancedRegion :: SourceLoc -> ScopeId -> UsagePattern -> EnhancedVisibilityRegion
createEnhancedRegion loc scopeId pattern =
  let pos = locPos loc
      -- Create base region with distinct positions based on scope
      baseRegion = VisibilityRegion $ Set.singleton $ ScopeRegion
        { scopeStart = pos
        , scopeEnd = pos + 10000 + fromIntegral (case scopeId of ScopeId n -> n)
        , scopeName = show scopeId
        }
  in EnhancedVisibilityRegion
      { baseRegion = baseRegion
      , scopeIds = Set.singleton scopeId  -- Definition scope only; descendants added later
      , usagePattern = pattern
      }

-- | Compute final visibility regions after scope tree is complete
-- Each binding is visible in its definition scope and all descendant scopes (lexical scoping)
computeFinalVisibility :: ScopeTree -> Map.Map BindingId EnhancedVisibilityRegion -> Map.Map BindingId EnhancedVisibilityRegion
computeFinalVisibility tree bindingMap =
  Map.mapWithKey (\_bindingId region -> 
    let defScopeIds = scopeIds region
        -- For each definition scope, include the scope itself and all its descendants
        -- getDescendants already includes the scope itself
        allVisibleScopes = Set.unions $ map (\defScope -> getDescendants tree defScope) (Set.toList defScopeIds)
    in region { scopeIds = allVisibleScopes }
  ) bindingMap

-- | Add binding to a scope node
addBindingToScope :: ScopeTree -> ScopeId -> BindingId -> ScopeTree
addBindingToScope tree scopeId bindingId =
  case Map.lookup scopeId (nodes tree) of
    Just node -> 
      let updatedNode = node { scopeBindings = Set.insert bindingId (scopeBindings node) }
      in tree { nodes = Map.insert scopeId updatedNode (nodes tree) }
    Nothing -> tree

-- | Get parent of current scope
parentOfScope :: ScopeTree -> Maybe ScopeId -> Maybe ScopeId
parentOfScope _ Nothing = Nothing
parentOfScope tree (Just scopeId) =
  case Map.lookup scopeId (nodes tree) of
    Just node -> parent node
    Nothing -> Nothing

-- | Get current control flow context
getCurrentContext :: EnhancedScopeState -> ControlContext
getCurrentContext state =
  case currentScope state of
    Just scopeId -> 
      case Map.lookup scopeId (nodes (scopeTree state)) of
        Just node -> case scopeType node of
          IfScope -> IfTrueBranch  -- Simplified: could track which branch
          LoopScope -> LoopBody
          LambdaScope -> NormalContext
          LetRecScope -> RecursiveCall
          _ -> NormalContext
        Nothing -> NormalContext
    Nothing -> NormalContext

