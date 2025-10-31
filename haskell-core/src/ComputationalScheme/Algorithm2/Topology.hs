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
import ComputationalScheme.Rig
import ComputationalScheme.Algorithm2.Scope (BindingScopeMap)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Zariski topology representation
-- Contains the open cover {D(f)} for all bindings f ∈ R_Scheme
data Topology = Topology
  { openSets :: Map.Map BindingId VisibilityRegion  -- ^ Map: f → D(f)
  , bindingCount :: Int                            -- ^ Number of bindings
  }
  deriving (Eq, Show)

-- | Build topology from R_Scheme and scope analysis
-- This is the main entry point for Algorithm 2
buildTopology :: RScheme -> BindingScopeMap -> Topology
buildTopology (RScheme bindings) scopeMap = Topology
  { openSets = Map.restrictKeys scopeMap bindings
  , bindingCount = Set.size bindings
  }

-- | Get visibility region D(f) for a binding
getVisibilityRegion :: Topology -> BindingId -> Maybe VisibilityRegion
getVisibilityRegion (Topology openSets _) bid = Map.lookup bid openSets

-- | Check if two bindings have overlapping visibility regions
haveOverlap :: Topology -> BindingId -> BindingId -> Bool
haveOverlap topo bid1 bid2 = case (getVisibilityRegion topo bid1, getVisibilityRegion topo bid2) of
  (Just (VisibilityRegion regions1), Just (VisibilityRegion regions2)) ->
    not $ Set.null $ Set.intersection regions1 regions2
  _ -> False

-- | Get all bindings in the topology
allBindings :: Topology -> Set.Set BindingId
allBindings (Topology openSets _) = Map.keysSet openSets

-- | Compute intersection of two visibility regions
intersectRegions :: VisibilityRegion -> VisibilityRegion -> VisibilityRegion
intersectRegions (VisibilityRegion r1) (VisibilityRegion r2) =
  VisibilityRegion $ Set.intersection r1 r2

-- | Check if intersection is non-empty
hasNonEmptyIntersection :: VisibilityRegion -> VisibilityRegion -> Bool
hasNonEmptyIntersection r1 r2 = 
  not $ Set.null $ regions (intersectRegions r1 r2)

