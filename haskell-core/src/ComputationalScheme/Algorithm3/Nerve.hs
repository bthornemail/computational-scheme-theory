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
hasIntersection :: VisibilityRegion -> VisibilityRegion -> Bool
hasIntersection (VisibilityRegion r1) (VisibilityRegion r2) =
  not $ Set.null $ Set.intersection r1 r2

-- | Check if three visibility regions have non-empty triple intersection
hasTripleIntersection :: VisibilityRegion -> VisibilityRegion -> VisibilityRegion -> Bool
hasTripleIntersection (VisibilityRegion r1) (VisibilityRegion r2) (VisibilityRegion r3) =
  not $ Set.null $ Set.intersection (Set.intersection r1 r2) r3

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

