{-# LANGUAGE OverloadedStrings #-}

-- | Nerve of Open Cover
--
-- The nerve of an open cover is a simplicial complex where:
-- - 0-simplices (vertices) correspond to open sets in the cover
-- - k-simplices correspond to (k+1)-way intersections of open sets
--
-- This is used to construct the Čech complex.
module ComputationalScheme.Algorithm3.Nerve where

import ComputationalScheme.Types
import ComputationalScheme.Algorithm2.OpenCover
import ComputationalScheme.Algorithm3.SimplicialComplex
import qualified Data.Set as Set
import qualified Data.List as List

-- | Compute the nerve of an open cover
-- The nerve N(𝒰) is a simplicial complex where a k-simplex exists
-- if and only if the intersection of the corresponding (k+1) open sets is non-empty.
computeNerve :: OpenCover -> SimplicialComplex
computeNerve cover = 
  let indices = [0..length cover - 1]
      -- 0-simplices: one for each open set
      vertices = Set.fromList [Simplex (Set.singleton i) | i <- indices]
      
      -- 1-simplices: pairs with non-empty intersection
      edges = Set.fromList $ 
        [(Simplex (Set.fromList [i, j])) | (i, j) <- allPairs indices, 
         hasIntersection (cover !! i) (cover !! j)]
      
      -- 2-simplices: triples with non-empty intersection
      triangles = Set.fromList $
        [(Simplex (Set.fromList [i, j, k])) | 
         (i, j, k) <- allTriples indices,
         hasIntersection (cover !! i) (cover !! j),
         hasIntersection (cover !! j) (cover !! k),
         hasIntersection (cover !! i) (cover !! k),
         hasTripleIntersection (cover !! i) (cover !! j) (cover !! k)]
      
      -- Higher simplices (for completeness, though we mainly need up to 2)
      higher = []  -- Can be extended if needed
      
  in SimplicialComplex vertices edges triangles higher

-- | Check if two visibility regions have non-empty intersection
-- Two regions overlap if any of their scope regions have overlapping position ranges
hasIntersection :: VisibilityRegion -> VisibilityRegion -> Bool
hasIntersection (VisibilityRegion r1) (VisibilityRegion r2) =
  -- Check if any region from r1 overlaps with any region from r2
  any (\reg1 -> any (regionsOverlap reg1) (Set.toList r2)) (Set.toList r1)
  where
    -- Two ScopeRegions overlap if their position ranges intersect
    regionsOverlap :: ScopeRegion -> ScopeRegion -> Bool
    regionsOverlap reg1 reg2 =
      max (scopeStart reg1) (scopeStart reg2) <= min (scopeEnd reg1) (scopeEnd reg2)

-- | Check if three visibility regions have non-empty triple intersection
-- All three regions must have at least one overlapping position range
hasTripleIntersection :: VisibilityRegion -> VisibilityRegion -> VisibilityRegion -> Bool
hasTripleIntersection (VisibilityRegion r1) (VisibilityRegion r2) (VisibilityRegion r3) =
  -- Check if there exists a triple of regions (one from each) that all overlap
  any (\reg1 -> any (\reg2 -> any (regionsAllOverlap reg1 reg2) (Set.toList r3)) (Set.toList r2)) (Set.toList r1)
  where
    -- Check if three regions all overlap pairwise
    regionsAllOverlap :: ScopeRegion -> ScopeRegion -> ScopeRegion -> Bool
    regionsAllOverlap reg1 reg2 reg3 =
      let overlap12 = max (scopeStart reg1) (scopeStart reg2) <= min (scopeEnd reg1) (scopeEnd reg2)
          overlap13 = max (scopeStart reg1) (scopeStart reg3) <= min (scopeEnd reg1) (scopeEnd reg3)
          overlap23 = max (scopeStart reg2) (scopeStart reg3) <= min (scopeEnd reg2) (scopeEnd reg3)
      in overlap12 && overlap13 && overlap23

-- | Generate all pairs from a list
allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | (x, i) <- zip xs [0..], (y, j) <- zip xs [0..], i < j]

-- | Generate all triples from a list
allTriples :: [a] -> [(a, a, a)]
allTriples xs = 
  [(x, y, z) | (x, i) <- zip xs [0..],
               (y, j) <- zip xs [0..],
               (z, k) <- zip xs [0..],
               i < j && j < k]

