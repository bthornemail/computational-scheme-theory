{-# LANGUAGE OverloadedStrings #-}

-- | Čech Complex Builder (Algorithm 3)
--
-- This module implements Algorithm 3: constructing the Čech complex
-- from the open cover {D(f_i)}.
--
-- The Čech complex is the nerve of the open cover, providing a simplicial
-- complex representation of the scope topology.
module ComputationalScheme.Algorithm3.CechComplex where

import ComputationalScheme.Types
import ComputationalScheme.Algorithm2.OpenCover
import ComputationalScheme.Algorithm2.Topology
import ComputationalScheme.Algorithm3.Nerve
import ComputationalScheme.Algorithm3.SimplicialComplex
import Data.Text (Text)

-- | Build Čech complex from topology
-- This is the main entry point for Algorithm 3.
--
-- Steps:
--   1. Extract open cover {D(f_i)} from topology
--   2. Compute nerve of the cover
--   3. Return simplicial complex
buildCechComplex :: Topology -> SimplicialComplex
buildCechComplex topo = 
  let cover = buildOpenCover topo
  in computeNerve cover

-- | Build Čech complex directly from open cover
-- Alternative entry point that takes the open cover directly
buildCechComplexFromCover :: OpenCover -> SimplicialComplex
buildCechComplexFromCover = computeNerve

-- | Get vertex count (number of 0-simplices)
-- This corresponds to the number of bindings in R_Scheme
vertexCount :: SimplicialComplex -> Int
vertexCount = Set.size . simplices0

-- | Get edge count (number of 1-simplices)
-- This corresponds to pairs of bindings with overlapping scopes
edgeCount :: SimplicialComplex -> Int
edgeCount = Set.size . simplices1

-- | Get triangle count (number of 2-simplices)
-- This corresponds to triples of bindings with overlapping scopes
triangleCount :: SimplicialComplex -> Int
triangleCount = Set.size . simplices2

-- | Compute Euler characteristic
-- χ = V - E + F - ... (alternating sum of simplex counts)
eulerCharacteristic :: SimplicialComplex -> Int
eulerCharacteristic complex =
  let v = Set.size (simplices0 complex)
      e = Set.size (simplices1 complex)
      f = Set.size (simplices2 complex)
      higher = sum [(-1)^(i+3) * Set.size (simplicesN complex !! (i-3)) | 
                    i <- [3..length (simplicesN complex) + 2]]
  in v - e + f + higher

-- | Check if complex is connected
-- A complex is connected if there exists a path between any two vertices
isConnected :: SimplicialComplex -> Bool
isConnected complex
  | Set.size (simplices0 complex) <= 1 = True
  | otherwise = 
      -- Use simple connectivity check: complex is connected if
      -- all vertices are reachable from vertex 0 via edges
      let vertices = Set.toList $ simplices0 complex
          edges = simplices1 complex
          reachable = reachableVertices (head vertices) edges Set.empty
      in Set.size reachable == Set.size (simplices0 complex)

-- | Find all vertices reachable from a given vertex
reachableVertices :: Simplex -> Set.Set Simplex -> Set.Set Simplex -> Set.Set Simplex
reachableVertices start edges visited
  | Set.member start visited = visited
  | otherwise = 
      let newVisited = Set.insert start visited
          -- Find all edges containing this vertex
          connected = Set.filter (\edge -> Set.member start (vertices edge)) edges
          -- Get all vertices connected via these edges
          neighbors = Set.unions $ map vertices (Set.toList connected)
          -- Recursively visit neighbors
      in foldl (\acc v -> reachableVertices v edges acc) newVisited (Set.toList neighbors)

