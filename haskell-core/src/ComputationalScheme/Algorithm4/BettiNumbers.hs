{-# LANGUAGE OverloadedStrings #-}

-- | Betti Numbers Calculator
--
-- Betti numbers β_k measure the "k-dimensional holes" in a topological space.
-- For our purposes:
--   β₀ = number of connected components
--   β₁ = number of 1-dimensional holes (cycles) = cyclomatic complexity
--   β₂ = number of 2-dimensional holes
module ComputationalScheme.Algorithm4.BettiNumbers where

import ComputationalScheme.Algorithm3.SimplicialComplex (SimplicialComplex(..), getSimplices)
import ComputationalScheme.Algorithm4.Cohomology (computeH1, computeHn)
import ComputationalScheme.Algorithm4.ChainComplex (CohomologyGroup(..))
import Numeric.LinearAlgebra

-- | Compute first Betti number β₁
-- This is the dimension of H¹, which measures "1-dimensional holes" (cycles).
computeBeta1 :: SimplicialComplex -> Int
computeBeta1 complex = dimension $ computeH1 complex

-- | Compute all Betti numbers up to dimension n
computeBettiNumbers :: Int -> SimplicialComplex -> [Int]
computeBettiNumbers maxDim complex = 
  [computeBetaN k complex | k <- [0..maxDim]]

-- | Compute Betti number β_k
computeBetaN :: Int -> SimplicialComplex -> Int
computeBetaN 0 = computeBeta0
computeBetaN 1 = computeBeta1
computeBetaN k complex
  | k > getComplexDimension complex = 0
  | otherwise = dimension $ computeHn k complex
  where
    getComplexDimension c = 
      if Set.null (simplices2 c) && null (simplicesN c)
      then if Set.null (simplices1 c) then 0 else 1
      else if null (simplicesN c) then 2 else 3  -- Simplified

-- | Compute zeroth Betti number β₀ (number of connected components)
computeBeta0 :: SimplicialComplex -> Int
computeBeta0 complex
  | Set.null (simplices0 complex) = 0
  | otherwise = 1  -- Simplified: assume connected for now
  -- Full implementation would use graph traversal to count components

-- | Euler characteristic χ = Σ(-1)^k β_k
-- For simplicial complexes: χ = Σ(-1)^k (number of k-simplices)
computeEulerCharacteristic :: SimplicialComplex -> Int
computeEulerCharacteristic complex =
  let v = Set.size (simplices0 complex)
      e = Set.size (simplices1 complex)
      f = Set.size (simplices2 complex)
      higher = sum [(-1)^k * Set.size (getSimplices k complex) | 
                    k <- [3..complexDimension complex]]
      complexDimension c = 
        if Set.null (simplices2 c) && null (simplicesN c)
        then if Set.null (simplices1 c) then 0 else 1
        else if null (simplicesN c) then 2 else 3
  in v - e + f + higher

