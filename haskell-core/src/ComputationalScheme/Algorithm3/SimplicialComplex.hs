{-# LANGUAGE DerivingStrategies #-}

-- | Simplicial Complex Data Structures
--
-- This module defines data structures for representing simplicial complexes,
-- which are the building blocks of Čech cohomology.
module ComputationalScheme.Algorithm3.SimplicialComplex where

import qualified Data.Set as Set
import Data.List (nub)

-- | A k-simplex (k+1 vertices)
-- A 0-simplex is a vertex, 1-simplex is an edge, 2-simplex is a triangle, etc.
newtype Simplex = Simplex { vertices :: Set.Set Int }
  deriving (Eq, Ord, Show)

-- | Dimension of simplex (number of vertices - 1)
-- 0-simplex (vertex) has dimension 0
-- 1-simplex (edge) has dimension 1
-- 2-simplex (triangle) has dimension 2
dimension :: Simplex -> Int
dimension (Simplex vs) = Set.size vs - 1

-- | Faces of a simplex (all proper subsets)
-- For a k-simplex, returns all (k-1)-simplices that are faces
faces :: Simplex -> Set.Set Simplex
faces (Simplex vs)
  | Set.size vs <= 1 = Set.empty
  | otherwise = Set.fromList
      [ Simplex (Set.delete v vs) | v <- Set.toList vs ]

-- | A simplicial complex
-- Contains simplices of various dimensions, with the closure property:
-- if a simplex is in the complex, all its faces must also be in the complex.
data SimplicialComplex = SimplicialComplex
  { simplices0 :: Set.Set Simplex  -- ^ 0-simplices (vertices)
  , simplices1 :: Set.Set Simplex  -- ^ 1-simplices (edges)
  , simplices2 :: Set.Set Simplex  -- ^ 2-simplices (triangles)
  , simplicesN :: [Set.Set Simplex] -- ^ Higher simplices (indexed by dimension)
  }
  deriving (Eq, Show)

-- | Empty simplicial complex
emptyComplex :: SimplicialComplex
emptyComplex = SimplicialComplex Set.empty Set.empty Set.empty []

-- | Check if complex is valid (all faces present)
-- A valid simplicial complex must satisfy the closure property:
-- if σ ∈ complex, then all faces of σ must also be in the complex.
isValid :: SimplicialComplex -> Bool
isValid complex = 
  -- Check that all faces of 1-simplices are 0-simplices
  let faceCheck1 = all (\e -> all (`Set.member` simplices0 complex) (Set.toList (faces e))) 
                       (Set.toList (simplices1 complex))
      -- Check that all faces of 2-simplices are 1-simplices
      faceCheck2 = all (\t -> all (`Set.member` simplices1 complex) (Set.toList (faces t))) 
                       (Set.toList (simplices2 complex))
      -- Check higher simplices
      faceCheckN = all (\(dim, simpls) -> 
        all (\s -> all (`Set.member` getSimplices (dim - 1) complex) (Set.toList (faces s))) 
            (Set.toList simpls))
        (zip [3..] (simplicesN complex))
  in faceCheck1 && faceCheck2 && faceCheckN

-- | Get simplices of a specific dimension
getSimplices :: Int -> SimplicialComplex -> Set.Set Simplex
getSimplices 0 complex = simplices0 complex
getSimplices 1 complex = simplices1 complex
getSimplices 2 complex = simplices2 complex
getSimplices n complex
  | n >= 3 && n - 3 < length (simplicesN complex) = (simplicesN complex) !! (n - 3)
  | otherwise = Set.empty

-- | Count simplices of a specific dimension
countSimplices :: Int -> SimplicialComplex -> Int
countSimplices dim complex = Set.size (getSimplices dim complex)

-- | Total number of simplices in the complex
totalSimplices :: SimplicialComplex -> Int
totalSimplices complex = 
  Set.size (simplices0 complex) +
  Set.size (simplices1 complex) +
  Set.size (simplices2 complex) +
  sum (map Set.size (simplicesN complex))

-- | Add a simplex to the complex (with closure)
addSimplex :: Simplex -> SimplicialComplex -> SimplicialComplex
addSimplex s complex
  | dimension s == 0 = complex { simplices0 = Set.insert s (simplices0 complex) }
  | dimension s == 1 = complex { simplices1 = Set.insert s (simplices1 complex) }
  | dimension s == 2 = complex { simplices2 = Set.insert s (simplices2 complex) }
  | otherwise = 
      let dim = dimension s
          idx = dim - 3
          higher = simplicesN complex
          -- Extend list if necessary
          extended = if idx < length higher 
                     then higher 
                     else higher ++ replicate (idx - length higher + 1) Set.empty
          updated = take idx extended ++ 
                    [Set.insert s (extended !! idx)] ++ 
                    drop (idx + 1) extended
      in complex { simplicesN = updated }

