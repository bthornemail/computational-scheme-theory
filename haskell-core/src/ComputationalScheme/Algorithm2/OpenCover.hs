{-# LANGUAGE OverloadedStrings #-}

-- | Open Cover Construction
--
-- This module provides utilities for working with the open cover {D(f)}
-- that forms the basis for the Čech complex construction (Algorithm 3).
module ComputationalScheme.Algorithm2.OpenCover where

import ComputationalScheme.Types
import ComputationalScheme.Algorithm2.Topology (Topology(..), getVisibilityRegion)
import ComputationalScheme.Algorithm2.ScopeTree
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List

-- | An open cover is just the collection of visibility regions
-- {D(f_i)} for all bindings f_i
type OpenCover = [VisibilityRegion]

-- | Enhanced open cover with scope tree information
data EnhancedOpenCover = EnhancedOpenCover
  { baseRegions :: [VisibilityRegion]              -- ^ Base regions for compatibility
  , enhancedRegionsList :: [EnhancedVisibilityRegion]  -- ^ Enhanced regions with tree info (renamed to avoid conflict)
  , coverScopeTree :: Maybe ScopeTree             -- ^ Scope tree for overlap detection
  }
  
-- | Helper accessor to avoid ambiguity
getEnhancedRegions :: EnhancedOpenCover -> [EnhancedVisibilityRegion]
getEnhancedRegions (EnhancedOpenCover { enhancedRegionsList = rs }) = rs

-- | Helper accessor to avoid ambiguity
getCoverScopeTree :: EnhancedOpenCover -> Maybe ScopeTree
getCoverScopeTree (EnhancedOpenCover { coverScopeTree = t }) = t

-- | Build open cover from topology
-- Extracts all visibility regions D(f) from the topology
buildOpenCover :: Topology -> OpenCover
buildOpenCover (Topology openSets _ _ _) = Map.elems openSets

-- | Build enhanced open cover from topology (with scope tree)
buildEnhancedOpenCover :: Topology -> EnhancedOpenCover
buildEnhancedOpenCover topo@(Topology openSets _ tree topoEnhancedMap) = 
  let coverEnhanced = Map.elems topoEnhancedMap
      baseRegs = Map.elems openSets
  in EnhancedOpenCover
      { baseRegions = baseRegs
      , enhancedRegionsList = coverEnhanced  
      , coverScopeTree = tree
      }

-- | Get pairs of open sets that have non-empty intersection
-- Used for constructing 1-simplices in Algorithm 3
intersectingPairs :: OpenCover -> [(VisibilityRegion, VisibilityRegion)]
intersectingPairs cover = 
  [(r1, r2) | (r1, i) <- zip cover [0..]
            , (r2, j) <- zip cover [0..]
            , i < j
            , hasNonEmptyIntersection r1 r2]

-- | Check if two visibility regions have non-empty intersection
-- Uses tree-based overlap if available, otherwise falls back to position-based
hasNonEmptyIntersection :: VisibilityRegion -> VisibilityRegion -> Bool
hasNonEmptyIntersection = hasNonEmptyIntersectionWithTree Nothing

-- | Check intersection with optional scope tree
hasNonEmptyIntersectionWithTree :: Maybe ScopeTree -> VisibilityRegion -> VisibilityRegion -> Bool
hasNonEmptyIntersectionWithTree _ (VisibilityRegion r1) (VisibilityRegion r2) =
  -- Fallback: position-based overlap
  any (\reg1 -> any (regionsOverlap reg1) (Set.toList r2)) (Set.toList r1)
  where
    -- Two ScopeRegions overlap if their position ranges intersect
    regionsOverlap :: ScopeRegion -> ScopeRegion -> Bool
    regionsOverlap reg1 reg2 =
      max (scopeStart reg1) (scopeStart reg2) <= min (scopeEnd reg1) (scopeEnd reg2)

-- | Check intersection of enhanced regions (uses tree-based if available)
hasNonEmptyIntersectionEnhanced :: EnhancedOpenCover -> Int -> Int -> Bool
hasNonEmptyIntersectionEnhanced cover i j =
  let coverEnhancedRegions = getEnhancedRegions cover
      coverTree = getCoverScopeTree cover
  in case (coverTree, 
        if i < length coverEnhancedRegions then Just (coverEnhancedRegions !! i) else Nothing,
        if j < length coverEnhancedRegions then Just (coverEnhancedRegions !! j) else Nothing) of
    (Just tree, Just r1, Just r2) ->
      -- Use tree-based overlap
      treeBasedOverlap tree r1 r2
    _ ->
      -- Fall back to position-based
      if i < length (baseRegions cover) && j < length (baseRegions cover)
        then hasNonEmptyIntersection ((baseRegions cover) !! i) ((baseRegions cover) !! j)
        else False

-- | Get triples of open sets that have non-empty intersection
-- Used for constructing 2-simplices in Algorithm 3
intersectingTriples :: OpenCover -> [(VisibilityRegion, VisibilityRegion, VisibilityRegion)]
intersectingTriples cover =
  [(r1, r2, r3) | (r1, i) <- zip cover [0..]
                 , (r2, j) <- zip cover [0..]
                 , (r3, k) <- zip cover [0..]
                 , i < j && j < k
                 , hasNonEmptyIntersection r1 r2
                 , hasNonEmptyIntersection r2 r3
                 , hasNonEmptyIntersection r1 r3]

-- | Compute intersection of all regions in a list
intersection :: [VisibilityRegion] -> VisibilityRegion
intersection [] = VisibilityRegion Set.empty
intersection [r] = r
intersection (r:rs) = foldl intersectRegions r rs
  where
    intersectRegions (VisibilityRegion r1) (VisibilityRegion r2) =
      VisibilityRegion $ Set.intersection r1 r2

