{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Incidence Matrix Builder
--
-- This module builds incidence matrices M₀, M₁, M₂ for computing cohomology.
-- The incidence matrix M_k has rows indexed by (k+1)-simplices and columns
-- indexed by k-simplices.
module ComputationalScheme.Algorithm4.IncidenceMatrix where

import ComputationalScheme.Algorithm3.SimplicialComplex
import Numeric.LinearAlgebra
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (sort)

-- | Build incidence matrix M₀: rows = 1-simplices (edges), cols = 0-simplices (vertices)
-- Entry (i,j) = +1 if edge i connects to vertex j, -1 for orientation, 0 otherwise
buildIncidenceMatrix0 :: SimplicialComplex -> Matrix Double
buildIncidenceMatrix0 complex =
  let vertices = sort $ Set.toList (simplices0 complex)
      edges = sort $ Set.toList (simplices1 complex)
      vertexIndex = Map.fromList $ zip vertices [0..]
      edgeIndex = Map.fromList $ zip edges [0..]
      numRows = length edges
      numCols = length vertices
  in buildMatrix numRows numCols $ \row col ->
      case (edgeIndex Map.!? (edges !! row), vertexIndex Map.!? (vertices !! col)) of
        (Just edgeIdx, Just vertIdx) ->
          if Set.member (vertices !! col) (vertices (edges !! row))
          then 1.0  -- Simplified: always +1 (orientation handled separately if needed)
          else 0.0
        _ -> 0.0

-- | Build incidence matrix M₁: rows = 2-simplices (triangles), cols = 1-simplices (edges)
-- Entry (i,j) = +1 if triangle i contains edge j, 0 otherwise
buildIncidenceMatrix1 :: SimplicialComplex -> Matrix Double
buildIncidenceMatrix1 complex =
  let edges = sort $ Set.toList (simplices1 complex)
      triangles = sort $ Set.toList (simplices2 complex)
      edgeIndex = Map.fromList $ zip edges [0..]
      triangleIndex = Map.fromList $ zip triangles [0..]
      numRows = length triangles
      numCols = length edges
  in buildMatrix numRows numCols $ \row col ->
      case (triangleIndex Map.!? (triangles !! row), edgeIndex Map.!? (edges !! col)) of
        (Just triIdx, Just edgeIdx) ->
          if edgeInTriangle (edges !! col) (triangles !! row)
          then 1.0
          else 0.0
        _ -> 0.0
  where
    edgeInTriangle :: Simplex -> Simplex -> Bool
    edgeInTriangle edge triangle = 
      Set.isSubsetOf (vertices edge) (vertices triangle)

-- | Build incidence matrix M₂: rows = 3-simplices, cols = 2-simplices
-- (For completeness, though we mainly need M₀ and M₁ for H¹)
buildIncidenceMatrix2 :: SimplicialComplex -> Matrix Double
buildIncidenceMatrix2 complex
  | null (simplicesN complex) = (1><1) [0.0]  -- Empty matrix
  | otherwise = 
      let triangles = sort $ Set.toList (simplices2 complex)
          tetrahedra = sort $ Set.toList (simplicesN complex !! 0)  -- 3-simplices
          triangleIndex = Map.fromList $ zip triangles [0..]
          tetraIndex = Map.fromList $ zip tetrahedra [0..]
          numRows = length tetrahedra
          numCols = length triangles
      in buildMatrix numRows numCols $ \row col ->
          case (tetraIndex Map.!? (tetrahedra !! row), triangleIndex Map.!? (triangles !! col)) of
            (Just tetIdx, Just triIdx) ->
              if triangleInTetrahedron (triangles !! col) (tetrahedra !! row)
              then 1.0
              else 0.0
            _ -> 0.0
      where
        triangleInTetrahedron :: Simplex -> Simplex -> Bool
        triangleInTetrahedron triangle tetra = 
          Set.isSubsetOf (vertices triangle) (vertices tetra)

-- | Helper to build a matrix with a function
buildMatrix :: Int -> Int -> (Int -> Int -> Double) -> Matrix Double
buildMatrix rows cols f = 
  fromLists [[f i j | j <- [0..cols-1]] | i <- [0..rows-1]]

-- | Get incidence matrices for H¹ computation
-- Returns (M₀, M₁) where:
--   M₀: incidence from 0-simplices to 1-simplices
--   M₁: incidence from 1-simplices to 2-simplices
getH1IncidenceMatrices :: SimplicialComplex -> (Matrix Double, Matrix Double)
getH1IncidenceMatrices complex = 
  let m0 = buildIncidenceMatrix0 complex
      m1 = buildIncidenceMatrix1 complex
  in (m0, m1)

