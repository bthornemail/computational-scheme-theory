{-# LANGUAGE OverloadedStrings #-}

-- | Cohomology Calculator (Algorithm 4)
--
-- This module implements Algorithm 4: computing Čech cohomology H¹
-- from the simplicial complex.
--
-- The formula: β₁ = (|N₁| - rank(M₁)) - rank(M₀)
-- where:
--   |N₁| = number of 1-simplices (edges)
--   rank(M₁) = rank of incidence matrix from edges to triangles
--   rank(M₀) = rank of incidence matrix from vertices to edges
module ComputationalScheme.Algorithm4.Cohomology where

import ComputationalScheme.Algorithm3.SimplicialComplex
import ComputationalScheme.Algorithm4.IncidenceMatrix
import ComputationalScheme.Algorithm4.ChainComplex
-- import ComputationalScheme.Algorithm4.BettiNumbers  -- Removed to break circular dependency
import Numeric.LinearAlgebra (rank, rows, cols, Matrix)
import qualified Data.Set as Set

-- | Compute first cohomology group H¹
-- This is the main entry point for Algorithm 4.
computeH1 :: SimplicialComplex -> CohomologyGroup
computeH1 complex = CohomologyGroup beta1
  where
    -- Build incidence matrices
    (m0, m1) = getH1IncidenceMatrices complex
    
    -- Compute ranks using hmatrix (handle empty matrices)
    rank0 = if rows m0 == 0 || cols m0 == 0 then 0 else rank m0
    rank1 = if rows m1 == 0 || cols m1 == 0 then 0 else rank m1
    
    -- Number of 1-simplices (edges)
    n1 = Set.size (simplices1 complex)
    
    -- β₁ = dim(cocycles) - dim(coboundaries)
    --    = (n₁ - rank(M₁)) - rank(M₀)
    -- where:
    --   n₁ - rank(M₁) = dimension of 1-cocycles (ker ∂*_2)
    --   rank(M₀) = dimension of 1-coboundaries (im ∂*_1)
    beta1 = max 0 ((n1 - rank1) - rank0)  -- Ensure non-negative

-- | Compute H¹ and return detailed result
computeH1Detailed :: SimplicialComplex -> H1Result
computeH1Detailed complex = 
  let (m0, m1) = getH1IncidenceMatrices complex
      rank0 = rank m0
      rank1 = rank m1
      n1 = Set.size (simplices1 complex)
      beta1 = (n1 - rank1) - rank0
      CohomologyGroup h1 = CohomologyGroup beta1
  in H1Result
      { h1Value = h1
      , numEdges = n1
      , rankM0 = rank0
      , rankM1 = rank1
      , incidenceM0 = m0
      , incidenceM1 = m1
      }

-- | Detailed result of H¹ computation
data H1Result = H1Result
  { h1Value :: Int              -- ^ β₁ = dim(H¹)
  , numEdges :: Int             -- ^ |N₁| = number of 1-simplices
  , rankM0 :: Int               -- ^ rank of M₀
  , rankM1 :: Int               -- ^ rank of M₁
  , incidenceM0 :: Matrix Double -- ^ Incidence matrix M₀ (for debugging)
  , incidenceM1 :: Matrix Double -- ^ Incidence matrix M₁ (for debugging)
  }
  deriving (Eq, Show)

-- | Compute higher cohomology groups (if needed)
computeHn :: Int -> SimplicialComplex -> CohomologyGroup
computeHn n complex
  | n < 0 = CohomologyGroup 0
  | n == 0 = computeH0 complex
  | n == 1 = computeH1 complex
  | otherwise = CohomologyGroup 0  -- Higher groups not implemented yet

-- | Compute H⁰ (connected components)
computeH0 :: SimplicialComplex -> CohomologyGroup
computeH0 complex = CohomologyGroup beta0
  where
    -- β₀ = number of connected components
    -- For now, simplified: β₀ = 1 if complex is non-empty
    beta0 = if Set.null (simplices0 complex) then 0 else 1

