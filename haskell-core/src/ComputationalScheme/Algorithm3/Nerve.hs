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
import Debug.Trace (trace, traceShow)

-- | Compute the nerve of an open cover
-- The nerve N(𝒰) is a simplicial complex where a k-simplex exists
-- if and only if the intersection of the corresponding (k+1) open sets is non-empty.
computeNerve :: OpenCover -> SimplicialComplex
computeNerve cover = 
  trace ("[Nerve] Computing nerve for cover of size " ++ show (length cover)) $
  let indices = [0..length cover - 1]
      -- 0-simplices: one for each open set
      vertices = trace ("[Nerve] Created " ++ show (length indices) ++ " 0-simplices (vertices)") $
                 Set.fromList [Simplex (Set.singleton i) | i <- indices]
      
      -- 1-simplices: pairs with non-empty intersection
      allPairsList = allPairs indices
      edges = trace ("[Nerve] Checking " ++ show (length allPairsList) ++ " pairs for intersection") $
              Set.fromList $ 
              [(Simplex (Set.fromList [i, j])) | (i, j) <- allPairsList, 
               let r1 = cover !! i
                   r2 = cover !! j
                   overlaps = hasIntersection r1 r2
               in trace ("[Nerve] Pair (" ++ show i ++ "," ++ show j ++ "): " ++ if overlaps then "OVERLAPS" else "no overlap") overlaps]
      
      edgesSize = Set.size edges
      _ = trace ("[Nerve] Created " ++ show edgesSize ++ " 1-simplices (edges)") edgesSize
      
      -- 2-simplices: triples with non-empty intersection
      allTriplesList = allTriples indices
      triangles = trace ("[Nerve] Checking " ++ show (length allTriplesList) ++ " triples for intersection") $
                  Set.fromList $
                  [(Simplex (Set.fromList [i, j, k])) | 
                   (i, j, k) <- allTriplesList,
                   let r1 = cover !! i
                       r2 = cover !! j
                       r3 = cover !! k
                       tripleOverlap = hasIntersection r1 r2 && hasIntersection r2 r3 && 
                                      hasIntersection r1 r3 && hasTripleIntersection r1 r2 r3
                   in trace ("[Nerve] Triple (" ++ show i ++ "," ++ show j ++ "," ++ show k ++ "): " ++ 
                            if tripleOverlap then "OVERLAPS" else "no overlap") tripleOverlap]
      
      trianglesSize = Set.size triangles
      _ = trace ("[Nerve] Created " ++ show trianglesSize ++ " 2-simplices (triangles)") trianglesSize
      
      -- Higher simplices (for completeness, though we mainly need up to 2)
      higher = []  -- Can be extended if needed
      
      result = SimplicialComplex vertices edges triangles higher
  in trace ("[Nerve] Nerve complete: " ++ show (Set.size vertices) ++ " vertices, " ++ 
            show (Set.size edges) ++ " edges, " ++ show (Set.size triangles) ++ " triangles") $
     result

-- | Compute the nerve of an enhanced open cover (with scope tree)
-- Uses tree-based overlap detection when scope tree is available
computeNerveEnhanced :: EnhancedOpenCover -> SimplicialComplex
computeNerveEnhanced cover = 
  trace ("[Nerve] Computing enhanced nerve for cover of size " ++ show (length (baseRegions cover))) $
  let indices = [0..length (baseRegions cover) - 1]
      -- 0-simplices: one for each open set
      vertices = trace ("[Nerve] Created " ++ show (length indices) ++ " 0-simplices (vertices)") $
                 Set.fromList [Simplex (Set.singleton i) | i <- indices]
      
      -- 1-simplices: pairs with non-empty intersection (using tree-based overlap)
      allPairsList = allPairs indices
      edges = trace ("[Nerve] Checking " ++ show (length allPairsList) ++ " pairs for intersection (tree-based)") $
              Set.fromList $ 
              [(Simplex (Set.fromList [i, j])) | (i, j) <- allPairsList, 
               let overlaps = hasNonEmptyIntersectionEnhanced cover i j
               in trace ("[Nerve] Pair (" ++ show i ++ "," ++ show j ++ "): " ++ if overlaps then "OVERLAPS" else "no overlap") overlaps]
      
      edgesSize = Set.size edges
      _ = trace ("[Nerve] Created " ++ show edgesSize ++ " 1-simplices (edges)") edgesSize
      
      -- 2-simplices: triples with non-empty intersection
      allTriplesList = allTriples indices
      triangles = trace ("[Nerve] Checking " ++ show (length allTriplesList) ++ " triples for intersection") $
                  Set.fromList $
                  [(Simplex (Set.fromList [i, j, k])) | 
                   (i, j, k) <- allTriplesList,
                   let tripleOverlap = hasNonEmptyIntersectionEnhanced cover i j && 
                                      hasNonEmptyIntersectionEnhanced cover j k && 
                                      hasNonEmptyIntersectionEnhanced cover i k &&
                                      hasTripleIntersectionEnhanced cover i j k
                   in trace ("[Nerve] Triple (" ++ show i ++ "," ++ show j ++ "," ++ show k ++ "): " ++ 
                            if tripleOverlap then "OVERLAPS" else "no overlap") tripleOverlap]
      
      trianglesSize = Set.size triangles
      _ = trace ("[Nerve] Created " ++ show trianglesSize ++ " 2-simplices (triangles)") trianglesSize
      
      -- Higher simplices
      higher = []
      
      result = SimplicialComplex vertices edges triangles higher
  in trace ("[Nerve] Enhanced nerve complete: " ++ show (Set.size vertices) ++ " vertices, " ++ 
            show (Set.size edges) ++ " edges, " ++ show (Set.size triangles) ++ " triangles") $
     result

-- | Check triple intersection for enhanced regions
hasTripleIntersectionEnhanced :: EnhancedOpenCover -> Int -> Int -> Int -> Bool
hasTripleIntersectionEnhanced cover i j k =
  hasNonEmptyIntersectionEnhanced cover i j &&
  hasNonEmptyIntersectionEnhanced cover j k &&
  hasNonEmptyIntersectionEnhanced cover i k

-- | Check if two visibility regions have non-empty intersection
-- Two regions overlap if any of their scope regions have overlapping position ranges
hasIntersection :: VisibilityRegion -> VisibilityRegion -> Bool
hasIntersection (VisibilityRegion r1) (VisibilityRegion r2) =
  let r1List = Set.toList r1
      r2List = Set.toList r2
      overlaps = any (\reg1 -> any (regionsOverlap reg1) r2List) r1List
  in trace ("[Nerve] hasIntersection: checking " ++ show (length r1List) ++ " vs " ++ show (length r2List) ++ 
            " regions -> " ++ if overlaps then "OVERLAP" else "no overlap") overlaps
  where
    -- Two ScopeRegions overlap if their position ranges intersect
    regionsOverlap :: ScopeRegion -> ScopeRegion -> Bool
    regionsOverlap reg1 reg2 =
      let startMax = max (scopeStart reg1) (scopeStart reg2)
          endMin = min (scopeEnd reg1) (scopeEnd reg2)
          overlaps = startMax <= endMin
      in if overlaps
         then trace ("[Nerve] regionsOverlap: OVERLAP " ++ 
                     "[" ++ scopeName reg1 ++ "] " ++ show (scopeStart reg1) ++ "-" ++ show (scopeEnd reg1) ++ 
                     " vs [" ++ scopeName reg2 ++ "] " ++ show (scopeStart reg2) ++ "-" ++ show (scopeEnd reg2)) $
             True
         else False

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

