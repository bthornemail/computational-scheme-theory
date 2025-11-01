{-# LANGUAGE OverloadedStrings #-}

-- | Open Cover Construction
--
-- This module provides utilities for working with the open cover {D(f)}
-- that forms the basis for the Čech complex construction (Algorithm 3).
module ComputationalScheme.Algorithm2.OpenCover where

import ComputationalScheme.Types
import ComputationalScheme.Algorithm2.Topology (Topology(..), getVisibilityRegion)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List

-- | An open cover is just the collection of visibility regions
-- {D(f_i)} for all bindings f_i
type OpenCover = [VisibilityRegion]

-- | Build open cover from topology
-- Extracts all visibility regions D(f) from the topology
buildOpenCover :: Topology -> OpenCover
buildOpenCover (Topology openSets _) = Map.elems openSets

-- | Get pairs of open sets that have non-empty intersection
-- Used for constructing 1-simplices in Algorithm 3
intersectingPairs :: OpenCover -> [(VisibilityRegion, VisibilityRegion)]
intersectingPairs cover = 
  [(r1, r2) | (r1, i) <- zip cover [0..]
            , (r2, j) <- zip cover [0..]
            , i < j
            , hasNonEmptyIntersection r1 r2]

-- | Check if two visibility regions have non-empty intersection
-- Two regions overlap if any of their scope regions have overlapping position ranges
hasNonEmptyIntersection :: VisibilityRegion -> VisibilityRegion -> Bool
hasNonEmptyIntersection (VisibilityRegion r1) (VisibilityRegion r2) =
  -- Check if any region from r1 overlaps with any region from r2
  any (\reg1 -> any (regionsOverlap reg1) (Set.toList r2)) (Set.toList r1)
  where
    -- Two ScopeRegions overlap if their position ranges intersect
    regionsOverlap :: ScopeRegion -> ScopeRegion -> Bool
    regionsOverlap reg1 reg2 =
      max (scopeStart reg1) (scopeStart reg2) <= min (scopeEnd reg1) (scopeEnd reg2)

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

