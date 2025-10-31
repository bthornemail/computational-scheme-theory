# Mathematical Core Architecture Design

**Status:** Planning Phase
**Language:** Haskell
**Purpose:** Implement the four required algorithms for computing H¹(X_Comp, O_Comp)

---

## 1. Technology Stack Selection

### Core Libraries

1. **Algebraic Structures**: `semirings` (Hackage)
   - Provides `Semiring` typeclass for rigs
   - Support for tropical semirings (max-plus algebra)
   - Active maintenance, works with recent GHC

2. **Linear Algebra**: `hmatrix` (Hackage)
   - Matrix operations for incidence matrices
   - Gaussian elimination for rank computation
   - BLAS/LAPACK backend for performance
   - Mature, widely used (Haskell's NumPy equivalent)

3. **Algebraic Topology**: Custom implementation + `at` (Effective Algebraic Topology)
   - `mvr/at` provides simplicial homology foundations
   - We'll implement Čech-specific constructions
   - Focus on cohomology (dual to homology)

4. **Parsing**: `megaparsec` for S-expression parsing
   - Error recovery for malformed input
   - Custom error messages

5. **Testing**: `QuickCheck` + `HUnit`
   - Property-based testing for algebraic laws
   - Unit tests for known examples

### Build System

- **Cabal** or **Stack** for dependency management
- **GHC 9.x** for latest type system features
- **Haddock** for API documentation

---

## 2. Module Structure

```
computational-scheme-theory/
├── haskell-core/
│   ├── src/
│   │   ├── ComputationalScheme/
│   │   │   ├── Types.hs                    -- Core type definitions
│   │   │   ├── Rig.hs                      -- R_Scheme rig definition
│   │   │   ├── Algorithm1/
│   │   │   │   ├── Parser.hs               -- S-expression parser
│   │   │   │   ├── AST.hs                  -- AST representation
│   │   │   │   ├── AlphaConversion.hs      -- Hygienic renaming
│   │   │   │   └── BindingExtractor.hs     -- Build R_Scheme
│   │   │   ├── Algorithm2/
│   │   │   │   ├── Topology.hs             -- Zariski topology
│   │   │   │   ├── Scope.hs                -- Visibility regions D(f)
│   │   │   │   └── OpenCover.hs            -- Open cover construction
│   │   │   ├── Algorithm3/
│   │   │   │   ├── Nerve.hs                -- Nerve of open cover
│   │   │   │   ├── SimplicialComplex.hs    -- Simplices and faces
│   │   │   │   └── CechComplex.hs          -- Čech complex builder
│   │   │   ├── Algorithm4/
│   │   │   │   ├── ChainComplex.hs         -- Chain/cochain complexes
│   │   │   │   ├── IncidenceMatrix.hs      -- Build M₀, M₁, M₂
│   │   │   │   ├── Cohomology.hs           -- Compute H¹
│   │   │   │   └── BettiNumbers.hs         -- β₁ calculation
│   │   │   ├── Distributed/
│   │   │   │   ├── TropicalRig.hs          -- (ℝ ∪ {-∞}, max, +)
│   │   │   │   ├── VectorClock.hs          -- Causal timestamps
│   │   │   │   └── Hypergraph.hs           -- Multi-party sync
│   │   │   ├── Service/
│   │   │   │   ├── GRPC.hs                 -- gRPC server
│   │   │   │   └── API.hs                  -- Service interface
│   │   │   └── Utils/
│   │   │       ├── Pretty.hs               -- Pretty printing
│   │   │       └── Debug.hs                -- Debugging utilities
│   │   └── Main.hs                          -- CLI entry point
│   ├── test/
│   │   ├── Algorithm1Spec.hs
│   │   ├── Algorithm2Spec.hs
│   │   ├── Algorithm3Spec.hs
│   │   ├── Algorithm4Spec.hs
│   │   └── Integration/
│   │       └── EndToEndSpec.hs
│   ├── bench/
│   │   └── Benchmarks.hs
│   ├── proto/
│   │   └── computational_scheme.proto       -- gRPC definitions
│   ├── computational-scheme-theory.cabal
│   └── README.md
```

---

## 3. Core Type Definitions

### 3.1 Binding Algebra (R_Scheme)

```haskell
-- Types.hs
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ComputationalScheme.Types where

import Data.Semiring (Semiring(..))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- | A binding identifier (after α-conversion)
newtype BindingId = BindingId { unBindingId :: String }
  deriving newtype (Eq, Ord, Show)

-- | The commutative rig R_Scheme
-- Elements are sets of bindings in scope
newtype RScheme = RScheme { bindings :: Set.Set BindingId }
  deriving stock (Eq, Ord, Show)

instance Semiring RScheme where
  -- Addition: scope union
  plus (RScheme a) (RScheme b) = RScheme (Set.union a b)

  -- Multiplication: scope nesting
  -- (for now, same as union; refinement may be needed)
  times (RScheme a) (RScheme b) = RScheme (Set.union a b)

  -- Zero: empty scope
  zero = RScheme Set.empty

  -- One: unit element (needs mathematical clarification)
  one = RScheme Set.empty

-- | A scope region in the program
data ScopeRegion = ScopeRegion
  { scopeStart :: !Int    -- Start position in source
  , scopeEnd   :: !Int    -- End position
  , scopeName  :: !String -- For debugging
  } deriving (Eq, Ord, Show)

-- | Visibility region D(f) - where binding f is in scope
newtype VisibilityRegion = VisibilityRegion
  { regions :: Set.Set ScopeRegion }
  deriving stock (Eq, Ord, Show)
```

### 3.2 Simplicial Complex

```haskell
-- Algorithm3/SimplicialComplex.hs
module ComputationalScheme.Algorithm3.SimplicialComplex where

import qualified Data.Set as Set

-- | A k-simplex (k+1 vertices)
newtype Simplex = Simplex { vertices :: Set.Set Int }
  deriving (Eq, Ord, Show)

-- | Dimension of simplex (number of vertices - 1)
dimension :: Simplex -> Int
dimension (Simplex vs) = Set.size vs - 1

-- | Faces of a simplex (all proper subsets)
faces :: Simplex -> Set.Set Simplex
faces (Simplex vs)
  | Set.size vs <= 1 = Set.empty
  | otherwise = Set.fromList
      [ Simplex (Set.delete v vs) | v <- Set.toList vs ]

-- | A simplicial complex
data SimplicialComplex = SimplicialComplex
  { simplices0 :: Set.Set Simplex  -- 0-simplices (vertices)
  , simplices1 :: Set.Set Simplex  -- 1-simplices (edges)
  , simplices2 :: Set.Set Simplex  -- 2-simplices (triangles)
  , simplicesN :: [Set.Set Simplex] -- Higher simplices
  } deriving (Eq, Show)

-- | Check if complex is valid (all faces present)
isValid :: SimplicialComplex -> Bool
isValid _ = undefined -- TODO: implement closure check
```

### 3.3 Cohomology

```haskell
-- Algorithm4/Cohomology.hs
module ComputationalScheme.Algorithm4.Cohomology where

import Numeric.LinearAlgebra (Matrix, rank)
import ComputationalScheme.Algorithm3.SimplicialComplex

-- | Incidence matrix between k-simplices and (k+1)-simplices
type IncidenceMatrix = Matrix Double

-- | Cohomology group (represented by dimension)
newtype CohomologyGroup = CohomologyGroup { dimension :: Int }
  deriving (Eq, Show)

-- | Compute first cohomology group H¹
computeH1 :: SimplicialComplex -> CohomologyGroup
computeH1 complex = CohomologyGroup beta1
  where
    -- Build incidence matrices
    m0 = buildIncidenceMatrix 0 complex  -- 0→1
    m1 = buildIncidenceMatrix 1 complex  -- 1→2

    -- Compute ranks
    rank0 = rank m0
    rank1 = rank m1

    -- Number of 1-simplices
    n1 = length (simplices1 complex)

    -- β₁ = dim(cocycles) - dim(coboundaries)
    --    = (n₁ - rank(M₁)) - rank(M₀)
    beta1 = (n1 - rank1) - rank0

buildIncidenceMatrix :: Int -> SimplicialComplex -> IncidenceMatrix
buildIncidenceMatrix k complex = undefined -- TODO: implement
```

---

## 4. Algorithm Designs

### Algorithm 1: Binding Algebra Extractor

**Input**: R5RS Scheme source code (S-expression string)
**Output**: R_Scheme (commutative rig of bindings)

**Steps**:
1. **Parse** S-expression → AST
2. **Identify binding forms**: `(lambda ...)`, `(let ...)`, `(define ...)`
3. **Apply α-conversion**: Generate unique names for all bindings
4. **Build rig**: Collect all unique bindings into R_Scheme
5. **Record scopes**: Track where each binding is visible

**Key Challenge**: Hygienic macro expansion (R5RS doesn't require it, but we need it)

**Design Decision**: Initially support a subset of R5RS without macros, then expand.

### Algorithm 2: Scope Topology Constructor

**Input**: R_Scheme with scope information
**Output**: Zariski topology τ_Scope (open cover {D(f)})

**Steps**:
1. For each binding `f ∈ R_Scheme`, compute its **visibility region D(f)**
2. D(f) = set of all program positions where `f` is in scope
3. Represent as set of `ScopeRegion` objects
4. Collection {D(f)} forms an open cover

**Key Insight**: This is a _finite_ topology (finitely many bindings)

### Algorithm 3: Čech Complex Builder

**Input**: Open cover {D(f_i)} from Algorithm 2
**Output**: Čech complex (simplicial complex)

**Steps**:
1. **0-simplices**: Each individual scope D(f_i) → vertex i
2. **1-simplices**: Pairs with non-empty intersection (D(f_i) ∩ D(f_j) ≠ ∅) → edge (i,j)
3. **2-simplices**: Triples with non-empty intersection (D(f_i) ∩ D(f_j) ∩ D(f_k) ≠ ∅) → triangle (i,j,k)
4. Continue for higher dimensions

**Implementation**: Iterate through all subsets, check intersections

### Algorithm 4: Cohomology Calculator

**Input**: Čech complex from Algorithm 3
**Output**: β₁ (first Betti number = dim(H¹))

**Steps**:
1. **Build incidence matrix M₀**: rows = 1-simplices, cols = 0-simplices
   - Entry = +1 or -1 if edge connects to vertex, 0 otherwise
2. **Build incidence matrix M₁**: rows = 2-simplices, cols = 1-simplices
   - Entry = +1 or -1 if triangle contains edge, 0 otherwise
3. **Compute ranks**: Use Gaussian elimination via `hmatrix`
4. **Calculate β₁**:
   ```
   β₁ = (|N₁| - rank(M₁)) - rank(M₀)
   ```
   where |N₁| = number of 1-simplices

**Key Property**: β₁ measures "1-dimensional holes" = cyclomatic complexity

---

## 5. gRPC Service Interface

```protobuf
// proto/computational_scheme.proto
syntax = "proto3";

package computational_scheme;

service MathematicalCore {
  // Compute H¹ for a Scheme program
  rpc ComputeCohomology(SchemeProgram) returns (CohomologyResult);

  // Extract binding algebra only
  rpc ExtractBindingAlgebra(SchemeProgram) returns (BindingAlgebra);

  // Health check
  rpc HealthCheck(Empty) returns (HealthStatus);
}

message SchemeProgram {
  string source_code = 1;
  string program_id = 2;
}

message CohomologyResult {
  int32 h0 = 1;  // β₀
  int32 h1 = 2;  // β₁ (our target)
  int32 h2 = 3;  // β₂
  repeated string bindings = 4;  // For debugging
  string error = 5;  // If computation failed
}

message BindingAlgebra {
  repeated Binding bindings = 1;
}

message Binding {
  string id = 1;
  string original_name = 2;
  ScopeRegion scope = 3;
}

message ScopeRegion {
  int32 start = 1;
  int32 end = 2;
  string name = 3;
}

message Empty {}

message HealthStatus {
  bool healthy = 1;
  string version = 2;
}
```

---

## 6. Testing Strategy

### Unit Tests

```haskell
-- test/Algorithm1Spec.hs
module Algorithm1Spec where

import Test.Hspec
import ComputationalScheme.Algorithm1.BindingExtractor

spec :: Spec
spec = describe "Binding Algebra Extractor" $ do

  it "extracts lambda bindings" $ do
    let source = "(lambda (x) x)"
    let rig = extractBindingAlgebra source
    length (bindings rig) `shouldBe` 1

  it "handles nested scopes" $ do
    let source = "(lambda (x) (lambda (y) (+ x y)))"
    let rig = extractBindingAlgebra source
    length (bindings rig) `shouldBe` 2

  it "applies α-conversion" $ do
    let source = "(lambda (x) (lambda (x) x))"
    let rig = extractBindingAlgebra source
    -- Both x's should get unique names
    length (bindings rig) `shouldBe` 2
```

### Property Tests

```haskell
-- test/Algorithm4Spec.hs
module Algorithm4Spec where

import Test.QuickCheck
import ComputationalScheme.Algorithm4.Cohomology

-- Property: β₁ ≥ 0 always
prop_bettiNonNegative :: SimplicialComplex -> Bool
prop_bettiNonNegative complex =
  let CohomologyGroup d = computeH1 complex
  in d >= 0

-- Property: β₁ = 0 for trees (acyclic)
prop_treesHaveNoCycles :: Tree -> Bool
prop_treesHaveNoCycles tree =
  let complex = treeToComplex tree
      CohomologyGroup beta1 = computeH1 complex
  in beta1 == 0
```

### Integration Tests

```haskell
-- test/Integration/EndToEndSpec.hs
module Integration.EndToEndSpec where

import Test.Hspec

spec :: Spec
spec = describe "End-to-end H¹ computation" $ do

  it "simple recursive function" $ do
    let source = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
    result <- computeH1FromSource source
    -- Should have β₁ = 1 (one cycle from recursion)
    h1 result `shouldBe` 1

  it "tail-recursive function (optimized)" $ do
    let source = "(define (sum-tail n acc) (if (= n 0) acc (sum-tail (- n 1) (+ acc n))))"
    result <- computeH1FromSource source
    -- Tail recursion: still β₁ = 1
    h1 result `shouldBe` 1
```

---

## 7. Performance Considerations

### Expected Complexity

- **Algorithm 1**: O(n) where n = AST size
- **Algorithm 2**: O(b²) where b = number of bindings
- **Algorithm 3**: O(2^b) worst case (intersection checks)
- **Algorithm 4**: O(s³) where s = number of simplices (Gaussian elimination)

### Optimization Strategies

1. **Lazy evaluation**: Only compute simplices up to dimension needed for H¹
2. **Sparse matrices**: Most incidence matrices are sparse
3. **Caching**: Memoize intersection computations
4. **Parallel**: Intersection checks are embarrassingly parallel

### Scalability Target

- Programs with **< 100 bindings**: milliseconds
- Programs with **100-1000 bindings**: seconds
- Programs with **> 1000 bindings**: may need approximation algorithms

---

## 8. Next Steps

1. **Set up Haskell project structure** (Cabal/Stack)
2. **Implement Types.hs** with core data structures
3. **Implement Algorithm 1** (parser + binding extractor)
4. **Write tests for Algorithm 1**
5. **Implement Algorithms 2-4** sequentially
6. **Add gRPC server**
7. **Performance benchmarking**

---

## 9. Open Questions

1. **Commutativity of R_Scheme multiplication**: How exactly should scope nesting compose?
2. **Higher-dimensional simplices**: Do we need to compute beyond 2-simplices for H¹?
3. **call/cc handling**: How does first-class continuations affect the binding algebra?
4. **Macro expansion**: Should we expand macros before or after α-conversion?

These questions will be resolved during implementation through empirical testing against known examples.
