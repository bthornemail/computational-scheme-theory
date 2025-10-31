{-# LANGUAGE OverloadedStrings #-}

-- | Chain and Cohain Complexes
--
-- This module provides data structures for representing chain and cochain
-- complexes, which are used to compute cohomology.
module ComputationalScheme.Algorithm4.ChainComplex where

import ComputationalScheme.Algorithm3.SimplicialComplex
import Numeric.LinearAlgebra (Matrix, Vector)
import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.List (sort)

-- | Chain complex representation
-- A chain complex consists of chain groups C_k and boundary maps ∂_k: C_k → C_{k-1}
data ChainComplex = ChainComplex
  { chainGroups :: [Set.Set Simplex]  -- ^ C_k for each dimension k
  , boundaryMaps :: [Matrix Double]   -- ^ Boundary matrices ∂_k
  }
  deriving (Eq, Show)

-- | Build chain complex from simplicial complex
buildChainComplex :: SimplicialComplex -> ChainComplex
buildChainComplex complex =
  let c0 = simplices0 complex
      c1 = simplices1 complex
      c2 = simplices2 complex
      cN = simplicesN complex
      chainGroups' = [c0, c1, c2] ++ cN
  in ChainComplex chainGroups' []  -- Boundary maps computed separately

-- | Dimension of chain complex (highest dimension with non-empty chain group)
complexDimension :: ChainComplex -> Int
complexDimension (ChainComplex groups _) = 
  length groups - 1  -- Assuming groups are indexed by dimension

-- | Get chain group of dimension k
getChainGroup :: Int -> ChainComplex -> Set.Set Simplex
getChainGroup k (ChainComplex groups _)
  | k >= 0 && k < length groups = groups !! k
  | otherwise = Set.empty

-- | Get boundary map of dimension k: ∂_k: C_k → C_{k-1}
getBoundaryMap :: Int -> ChainComplex -> Matrix Double
getBoundaryMap k (ChainComplex _ maps)
  | k >= 0 && k < length maps = maps !! k
  | otherwise = error $ "Boundary map not computed for dimension " ++ show k

-- | Cohomology group (represented by dimension)
-- H^k = ker(∂*_{k+1}) / im(∂*_k)
-- where ∂* is the coboundary operator (dual to boundary)
newtype CohomologyGroup = CohomologyGroup { dimension :: Int }
  deriving (Eq, Ord, Show)

-- | First cohomology group H¹
-- This is our primary target for Algorithm 4
type H1 = CohomologyGroup

