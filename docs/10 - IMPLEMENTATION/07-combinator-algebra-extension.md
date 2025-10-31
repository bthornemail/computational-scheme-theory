# Combinator Algebra Extension - Implementation Design

**Status:** Planning Phase (Extension)
**Priority:** Phase 2 (after core validation)
**Purpose:** Implement Y-combinator rings and Z-combinator fields for recursive computation and distributed consensus

---

## 1. Overview

This document extends our Haskell mathematical core with combinator algebra support as specified in:
- `RFCXXXX - Computational Scheme Theory Protocol Specification - Appendix Z`

The combinator extension enables:
1. **Recursive computation** via Y-combinator rings
2. **Distributed consensus** via Z-combinator fixed-point fields
3. **Automatonic flow** for self-describing recursive systems

---

## 2. Module Structure Extension

Add to existing Haskell core:

```
haskell-core/
├── src/
│   └── ComputationalScheme/
│       ├── Combinator/                    # NEW
│       │   ├── Types.hs                   # Combinator type definitions
│       │   ├── YCombinator.hs             # Y-combinator ring
│       │   ├── ZCombinator.hs             # Z-combinator field
│       │   ├── FixedPoint.hs              # Fixed-point finding
│       │   ├── Consensus.hs               # Combinator consensus
│       │   └── Service.hs                 # gRPC service extension
│       ├── FSM.hs                         # EXTEND with combinator state
│       └── Service/GRPC.hs                # EXTEND with combinator RPCs
```

---

## 3. Core Type Definitions

### 3.1 Y-Combinator Ring

```haskell
-- Combinator/Types.hs
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module ComputationalScheme.Combinator.Types where

import qualified Data.Map.Strict as Map

-- | Identifier for combinator rings/fields
newtype RingId = RingId { unRingId :: String }
  deriving (Eq, Ord, Show)

newtype FieldId = FieldId { unFieldId :: String }
  deriving (Eq, Ord, Show)

-- | Y-combinator: fixed-point combinator for recursion
-- Satisfies: Y f = f (Y f)
newtype YCombinator = YCombinator {
  runY :: forall a. (a -> a) -> a
}

-- | Generator function for recursive structures
newtype Generator r = Generator {
  generate :: r -> r
}

-- | Y-combinator ring structure
data YCombinatorRing r = YCombinatorRing {
  ringId :: RingId
  , baseRing :: r                      -- Underlying ring R
  , yCombinator :: YCombinator          -- Y combinator
  , recursiveStructure :: Generator r   -- Recursive structure builder
} deriving (Show)

-- | Fixed point computed by Y-combinator
data FixedPoint r = FixedPoint {
  generator :: Generator r
  , fixedPointValue :: r
  , iterations :: Maybe Int  -- For Z-combinator (iterative)
} deriving (Show)
```

### 3.2 Z-Combinator Field

```haskell
-- Combinator/Types.hs (continued)

-- | Z-combinator: strict fixed-point combinator
-- Satisfies: Z f = f (Z f) with strict evaluation
newtype ZCombinator = ZCombinator {
  runZ :: forall a b. ((a -> b) -> (a -> b)) -> (a -> b)
}

-- | Z-combinator field structure
data ZCombinatorField f = ZCombinatorField {
  fieldId :: FieldId
  , baseField :: f                     -- Underlying field F
  , zCombinator :: ZCombinator          -- Z combinator
  , fixedPointFinder :: FixedPointFinder f
  , iterativeRefinement :: IterativeRefinement f
} deriving (Show)

-- | Fixed-point finder (iterative)
data FixedPointFinder f = FixedPointFinder {
  targetFunction :: f -> f
  , tolerance :: Double
  , maxIterations :: Int
}

-- | Iterative refinement for convergence
data IterativeRefinement f = IterativeRefinement {
  refineStep :: f -> f -> f  -- (current, next) -> refined
  , convergenceTest :: f -> f -> Bool
}
```

### 3.3 Consensus Structures

```haskell
-- Combinator/Consensus.hs
module ComputationalScheme.Combinator.Consensus where

import ComputationalScheme.Combinator.Types

-- | Node in distributed system
data Node = Node {
  nodeId :: String
  , nodeState :: NodeState
} deriving (Eq, Show)

data NodeState = NodeState {
  localValue :: String  -- Simplified; would be polymorphic
  , vectorClock :: Map String Int
} deriving (Eq, Show)

-- | Consensus protocol types
data ConsensusType
  = ZFieldConsensus FieldId [Node]
  | YRingConsensus RingId [Node]
  | ZYConsensus FieldId RingId [Node]
  deriving (Eq, Show)

-- | Consensus result
data ConsensusResult = ConsensusResult {
  finalState :: String  -- Agreed-upon value
  , iterationCount :: Int
  , convergenceTime :: Double
  , success :: Bool
  , failureReason :: Maybe String
} deriving (Eq, Show)
```

---

## 4. Y-Combinator Implementation

```haskell
-- Combinator/YCombinator.hs
module ComputationalScheme.Combinator.YCombinator where

import ComputationalScheme.Combinator.Types

-- | The Y-combinator (call-by-name version)
-- Y f = f (Y f)
yCombinator :: (a -> a) -> a
yCombinator f = f (yCombinator f)

-- | Create a Y-combinator ring
createYCombinatorRing :: RingId -> r -> Generator r -> YCombinatorRing r
createYCombinatorRing ringId baseRing generator = YCombinatorRing {
  ringId = ringId
  , baseRing = baseRing
  , yCombinator = YCombinator yCombinator
  , recursiveStructure = generator
}

-- | Compute recursive structure using Y-combinator
computeRecursiveStructure :: YCombinatorRing r -> r
computeRecursiveStructure ring =
  let YCombinator y = yCombinator ring
      Generator g = recursiveStructure ring
  in y g

-- | Example: Factorial using Y-combinator
factorialY :: Integer -> Integer
factorialY = runY (YCombinator yCombinator) $ \fact n ->
  if n == 0
    then 1
    else n * fact (n - 1)

-- | Example: Fibonacci using Y-combinator
fibonacciY :: Integer -> Integer
fibonacciY = runY (YCombinator yCombinator) $ \fib n ->
  if n <= 1
    then n
    else fib (n - 1) + fib (n - 2)
```

---

## 5. Z-Combinator Implementation

```haskell
-- Combinator/ZCombinator.hs
module ComputationalScheme.Combinator.ZCombinator where

import ComputationalScheme.Combinator.Types
import Control.Monad.State

-- | The Z-combinator (call-by-value version with strictness)
-- Z f = f (\x -> Z f x)
zCombinator :: ((a -> b) -> (a -> b)) -> (a -> b)
zCombinator f = f (\x -> zCombinator f x)

-- | Create a Z-combinator field
createZCombinatorField :: FieldId -> f -> FixedPointFinder f -> IterativeRefinement f -> ZCombinatorField f
createZCombinatorField fieldId baseField finder refinement = ZCombinatorField {
  fieldId = fieldId
  , baseField = baseField
  , zCombinator = ZCombinator zCombinator
  , fixedPointFinder = finder
  , iterativeRefinement = refinement
}

-- | Find fixed point using iterative refinement
findFixedPoint :: (Eq f, Num f) => ZCombinatorField f -> (f -> f) -> f -> Either String (FixedPoint f)
findFixedPoint field func initial = do
  let finder = fixedPointFinder field
      maxIter = maxIterations finder
      tol = tolerance finder

  runIterations func initial 0 maxIter tol
  where
    runIterations f current iter maxIter tol
      | iter >= maxIter = Left $ "Max iterations (" ++ show maxIter ++ ") exceeded"
      | otherwise = do
          let next = f current
              diff = abs (next - current)

          if diff < tol
            then Right $ FixedPoint {
              generator = Generator f
              , fixedPointValue = next
              , iterations = Just iter
            }
            else runIterations f next (iter + 1) maxIter tol

-- | Example: Square root via fixed point (Newton's method)
sqrtViaFixedPoint :: Double -> Double
sqrtViaFixedPoint n =
  let field = createZCombinatorField
        (FieldId "sqrt-field")
        (0 :: Double)
        (FixedPointFinder (\x -> (x + n/x) / 2) 0.0001 100)
        (IterativeRefinement (\_ next -> next) (\curr next -> abs (curr - next) < 0.0001))
  in case findFixedPoint field (\x -> (x + n/x) / 2) 1.0 of
       Right fp -> fixedPointValue fp
       Left err -> error err
```

---

## 6. FSM Integration

```haskell
-- FSM.hs (EXTENDED)
module ComputationalScheme.FSM where

import qualified Data.Map.Strict as Map
import ComputationalScheme.Combinator.Types
import ComputationalScheme.Types  -- Existing types

-- | Extended FSM state with combinator algebras
data FSMState = FSMState {
  -- Existing fields
  bindingAlgebra :: RScheme
  , eventStore :: [SExpression]
  , vectorClock :: VectorClock

  -- NEW: Combinator fields
  , yCombinatorRings :: Map RingId (YCombinatorRing RScheme)
  , zCombinatorFields :: Map FieldId (ZCombinatorField Double)  -- Simplified
} deriving (Show)

-- | Extended transition rules
data TransitionRule
  = -- Existing rules
    ValidateProgram ProgramId
  | ComputeH1 ProgramId

  | -- NEW: Combinator rules
    CreateYCombinatorRing RingId RScheme Generator
  | CreateZCombinatorField FieldId Double FixedPointFinder IterativeRefinement
  | ComputeRecursiveStructure RingId
  | FindFixedPoint FieldId (Double -> Double)
  | CombinatorConsensus ConsensusType
  deriving (Show)

-- | Process combinator commands
processCombinatorCommand :: FSMState -> MExpression -> Either Error (FSMState, [SExpression])
processCombinatorCommand state mExpr = case mExpr of
  CreateYCombinatorRing ringId baseRing generator -> do
    let yRing = createYCombinatorRing ringId baseRing generator
        newState = state {
          yCombinatorRings = Map.insert ringId yRing (yCombinatorRings state)
        }
        event = SExpr $ YRingCreated ringId baseRing currentTime
    pure (newState, [event])

  ComputeRecursiveStructure ringId -> do
    yRing <- maybeToEither (RingNotFound ringId) $
               Map.lookup ringId (yCombinatorRings state)
    let result = computeRecursiveStructure yRing
        event = SExpr $ RecursiveStructureComputed ringId result currentTime
    pure (state, [event])

  FindFixedPoint fieldId func -> do
    zField <- maybeToEither (FieldNotFound fieldId) $
                Map.lookup fieldId (zCombinatorFields state)
    case findFixedPoint zField func 1.0 of
      Right fp -> do
        let event = SExpr $ FixedPointFound fieldId (fixedPointValue fp) (iterations fp) currentTime
        pure (state, [event])
      Left err -> Left (FixedPointError err)

  _ -> Left (UnknownCommand mExpr)
```

---

## 7. gRPC Service Extension

```haskell
-- Combinator/Service.hs
module ComputationalScheme.Combinator.Service where

import qualified Proto.CombinatorAlgebra as Proto
import ComputationalScheme.Combinator.YCombinator
import ComputationalScheme.Combinator.ZCombinator
import ComputationalScheme.Combinator.Consensus

-- | Combinator algebra service implementation
combinatorAlgebraService :: Service CombinatorAlgebra ServerRequest ServerResponse
combinatorAlgebraService = Service {
  createYCombinatorRing = createYRingHandler
  , createZCombinatorField = createZFieldHandler
  , computeRecursiveStructure = computeRecursiveHandler
  , findFixedPoint = findFixedPointHandler
  , combinatorConsensus = consensusHandler
}

-- | Handler implementations
createYRingHandler :: Proto.YRingRequest -> IO Proto.YRingResponse
createYRingHandler req = do
  let ringId = RingId (Proto.name req)
      -- Create Y-combinator ring
      yRing = createYCombinatorRing ringId baseRing generator

  return $ Proto.YRingResponse {
    ringId = Proto.name req
    , success = True
    , error = Nothing
  }

findFixedPointHandler :: Proto.FixedPointRequest -> IO Proto.FixedPointResponse
findFixedPointHandler req = do
  let fieldId = FieldId (Proto.fieldName req)
      -- Find fixed point

  case findFixedPoint zField func initial of
    Right fp -> return $ Proto.FixedPointResponse {
      fieldId = Proto.fieldName req
      , fixedPointValue = fixedPointValue fp
      , iterations = fromMaybe 0 (iterations fp)
      , success = True
      , error = Nothing
    }
    Left err -> return $ Proto.FixedPointResponse {
      success = False
      , error = Just err
    }
```

---

## 8. Protocol Buffer Extensions

```protobuf
// proto/combinator_algebra.proto
syntax = "proto3";

package computational_scheme.combinator;

import "common.proto";
import "math_core.proto";

service CombinatorAlgebra {
  rpc CreateYCombinatorRing(YRingRequest) returns (YRingResponse);
  rpc CreateZCombinatorField(ZFieldRequest) returns (ZFieldResponse);
  rpc ComputeRecursiveStructure(RecursiveRequest) returns (RecursiveResponse);
  rpc FindFixedPoint(FixedPointRequest) returns (FixedPointResponse);
  rpc CombinatorConsensus(ConsensusRequest) returns (ConsensusResponse);
}

message YRingRequest {
  string name = 1;
  Ring base_ring = 2;
  Generator generator = 3;
}

message YRingResponse {
  string ring_id = 1;
  bool success = 2;
  string error = 3;
}

message ZFieldRequest {
  string name = 1;
  Field base_field = 2;
  FixedPointFinder finder = 3;
}

message ZFieldResponse {
  string field_id = 1;
  bool success = 2;
  string error = 3;
}

message FixedPointRequest {
  string field_name = 1;
  Function target_function = 2;
  double initial_value = 3;
}

message FixedPointResponse {
  string field_id = 1;
  double fixed_point_value = 2;
  int32 iterations = 3;
  bool success = 4;
  string error = 5;
}
```

---

## 9. Testing Strategy

### 9.1 Y-Combinator Tests

```haskell
-- test/Combinator/YCombinatorSpec.hs
module Combinator.YCombinatorSpec where

import Test.Hspec
import Test.QuickCheck
import ComputationalScheme.Combinator.YCombinator

spec :: Spec
spec = describe "Y-Combinator" $ do

  it "computes factorial correctly" $ do
    factorialY 0 `shouldBe` 1
    factorialY 5 `shouldBe` 120
    factorialY 10 `shouldBe` 3628800

  it "computes fibonacci correctly" $ do
    fibonacciY 0 `shouldBe` 0
    fibonacciY 1 `shouldBe` 1
    fibonacciY 10 `shouldBe` 55

  it "satisfies fixed-point property: Y f = f (Y f)" $ property $
    \n -> n >= 0 && n < 20 ==>
      let f fact = \x -> if x == 0 then 1 else x * fact (x - 1)
          yF = runY (YCombinator yCombinator) f
      in yF n == f yF n
```

### 9.2 Z-Combinator Tests

```haskell
-- test/Combinator/ZCombinatorSpec.hs
module Combinator.ZCombinatorSpec where

import Test.Hspec
import ComputationalScheme.Combinator.ZCombinator

spec :: Spec
spec = describe "Z-Combinator" $ do

  it "finds square root via fixed point" $ do
    let sqrt4 = sqrtViaFixedPoint 4.0
    sqrt4 `shouldSatisfy` (\x -> abs (x - 2.0) < 0.001)

  it "converges within iteration limit" $ do
    let field = createZCombinatorField
          (FieldId "test")
          (0 :: Double)
          (FixedPointFinder (\x -> (x + 4/x) / 2) 0.0001 10)
          (IterativeRefinement (\_ next -> next) (\curr next -> abs (curr - next) < 0.0001))

    case findFixedPoint field (\x -> (x + 4/x) / 2) 1.0 of
      Right fp -> iterations fp `shouldSatisfy` (\i -> i < Just 10)
      Left _ -> expectationFailure "Should converge"
```

---

## 10. Integration with Core Validation

### Phase Integration

**Phase 1** (Months 1-4): **NOT INCLUDED** - Focus on core H¹ validation

**Phase 2** (Months 5-8): **OPTIONAL** - Add if time permits

**Phase 3** (Months 9-12): **INCLUDE** - For distributed consensus via Z-combinator

**Phase 4** (Months 13-16): **EXTEND** - Use for recursive NLI queries

### Use Cases in Validation

1. **Recursive Program Analysis** (Y-combinator):
   - Test corpus includes recursive programs
   - Y-combinator ring models recursive binding structure
   - Enables compositional analysis of recursive complexity

2. **Distributed Validation** (Z-combinator):
   - Consensus across multiple validation nodes
   - Fixed-point convergence for agreeing on H¹ values
   - Useful for Phase 3 distributed architecture

---

## 11. Open Questions

1. **Performance**: Will lazy Y-combinator cause stack overflow? May need memoization.
2. **Termination**: How to guarantee Z-combinator convergence? Need contractive map proofs.
3. **Integration**: How do combinator rings interact with R_Scheme rig? Need category-theoretic clarification.

---

## 12. Next Steps

**When to implement**: After Phase 1 validation succeeds

**Priority**: Medium (enhances but not required for core hypothesis)

**Tasks**:
1. Implement Y-combinator ring types
2. Implement Z-combinator field types
3. Extend FSM with combinator state
4. Add gRPC service methods
5. Write comprehensive tests
6. Document usage in validation corpus

---

## Conclusion

This extension adds Y-combinator and Z-combinator abstractions to our Haskell implementation, enabling:
- Recursive program analysis via Y-combinator rings
- Distributed consensus via Z-combinator fields
- Automatonic flow for self-describing systems

The design follows Appendix Z specifications and integrates cleanly with our existing architecture.

**Status**: ✅ Design Complete - Ready to implement in Phase 2+
