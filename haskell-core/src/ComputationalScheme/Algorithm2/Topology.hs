{-# LANGUAGE OverloadedStrings #-}

-- | Zariski Topology Constructor (Algorithm 2)
--
-- This module implements Algorithm 2: constructing the Zariski topology
-- τ_Scope from the binding algebra R_Scheme.
--
-- The topology is constructed from the collection {D(f)} where D(f) is
-- the visibility region of binding f.
module ComputationalScheme.Algorithm2.Topology where

import ComputationalScheme.Types
import ComputationalScheme.Rig hiding (bindingCount)
import ComputationalScheme.Algorithm2.Scope (BindingScopeMap)
import ComputationalScheme.Algorithm2.ScopeTree
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Zariski topology representation
-- Contains the open cover {D(f)} for all bindings f ∈ R_Scheme
-- Can work with either old VisibilityRegion or new EnhancedVisibilityRegion
data Topology = Topology
  { openSets :: Map.Map BindingId VisibilityRegion  -- ^ Map: f → D(f) (base regions)
  , bindingCount :: Int                            -- ^ Number of bindings
  , scopeTree :: Maybe ScopeTree                   -- ^ Optional scope tree for tree-based overlap
  , enhancedRegions :: Map.Map BindingId EnhancedVisibilityRegion  -- ^ Enhanced regions if available
  }
  deriving (Eq, Show)

-- | Build topology from R_Scheme and scope analysis (old method)
-- This is the main entry point for Algorithm 2
buildTopology :: RScheme -> BindingScopeMap -> Topology
buildTopology (RScheme bindings) scopeMap = Topology
  { openSets = Map.restrictKeys scopeMap bindings
  , bindingCount = Set.size bindings
  , scopeTree = Nothing
  , enhancedRegions = Map.empty
  } where
    -- bindingCount is the field name, not the function from Rig

-- | Build topology from R_Scheme and enhanced scope analysis (with scope tree)
buildTopologyEnhanced :: RScheme -> Map.Map BindingId EnhancedVisibilityRegion -> ScopeTree -> Topology
buildTopologyEnhanced (RScheme bindings) enhancedMap tree = Topology
  { openSets = Map.map baseRegion (Map.restrictKeys enhancedMap bindings)
  , bindingCount = Set.size bindings
  , scopeTree = Just tree
  , enhancedRegions = Map.restrictKeys enhancedMap bindings
  }

-- | Get visibility region D(f) for a binding
getVisibilityRegion :: Topology -> BindingId -> Maybe VisibilityRegion
getVisibilityRegion (Topology openSets _ _ _) bid = Map.lookup bid openSets

-- | Check if two bindings have overlapping visibility regions
-- Uses tree-based overlap if scope tree is available, otherwise falls back to position-based
haveOverlap :: Topology -> BindingId -> BindingId -> Bool
haveOverlap topo bid1 bid2 = 
  case (scopeTree topo, Map.lookup bid1 (enhancedRegions topo), Map.lookup bid2 (enhancedRegions topo)) of
    (Just tree, Just r1, Just r2) -> 
      -- Use tree-based overlap
      treeBasedOverlap tree r1 r2
    _ -> 
      -- Fall back to position-based overlap
      case (getVisibilityRegion topo bid1, getVisibilityRegion topo bid2) of
        (Just (VisibilityRegion regions1), Just (VisibilityRegion regions2)) ->
          not $ Set.null $ Set.intersection regions1 regions2
        _ -> False

-- | Get all bindings in the topology
allBindings :: Topology -> Set.Set BindingId
allBindings (Topology openSets _ _ _) = Map.keysSet openSets

-- | Compute intersection of two visibility regions
intersectRegions :: VisibilityRegion -> VisibilityRegion -> VisibilityRegion
intersectRegions (VisibilityRegion r1) (VisibilityRegion r2) =
  VisibilityRegion $ Set.intersection r1 r2

-- | Check if intersection is non-empty
hasNonEmptyIntersection :: VisibilityRegion -> VisibilityRegion -> Bool
hasNonEmptyIntersection r1 r2 = 
  not $ Set.null $ regions (intersectRegions r1 r2)

