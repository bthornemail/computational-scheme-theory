{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Core type definitions for Computational Scheme Theory
--
-- This module defines the fundamental types for representing Scheme programs
-- as algebraic structures, specifically the commutative rig R_Scheme.
module ComputationalScheme.Types where

import Data.Semiring (Semiring(..))
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | A binding identifier (after α-conversion)
-- Each binding gets a unique identifier to handle shadowing and hygiene
newtype BindingId = BindingId { unBindingId :: Text }
  deriving newtype (Eq, Ord, Show)

-- | The commutative rig R_Scheme
-- Elements are sets of bindings in scope.
-- This forms the algebraic structure underlying the computational scheme.
--
-- Properties:
--   - Addition: union of scopes (plus)
--   - Multiplication: scope nesting (times)
--   - Zero: empty scope
--   - One: unit element (currently empty, may need refinement)
newtype RScheme = RScheme { bindings :: Set.Set BindingId }
  deriving stock (Eq, Ord, Show)

instance Semiring RScheme where
  -- Addition: scope union
  -- The union of two scopes contains all bindings from both
  plus (RScheme a) (RScheme b) = RScheme (Set.union a b)

  -- Multiplication: scope nesting
  -- For now, same as union; refinement may be needed based on empirical results
  times (RScheme a) (RScheme b) = RScheme (Set.union a b)

  -- Zero: empty scope (additive identity)
  zero = RScheme Set.empty

  -- One: unit element (multiplicative identity)
  -- Currently same as zero; mathematical properties need verification
  one = RScheme Set.empty

-- | A scope region in the program
-- Represents a contiguous region where a binding is visible
data ScopeRegion = ScopeRegion
  { scopeStart :: !Int    -- ^ Start position in source code
  , scopeEnd   :: !Int    -- ^ End position in source code
  , scopeName  :: !String -- ^ Descriptive name for debugging
  }
  deriving (Eq, Ord, Show)

-- | Visibility region D(f) - where binding f is in scope
-- This is a collection of scope regions where a particular binding is visible.
-- Used to construct the Zariski topology in Algorithm 2.
newtype VisibilityRegion = VisibilityRegion
  { regions :: Set.Set ScopeRegion }
  deriving stock (Eq, Ord, Show)

-- | Scope identifier for tree structure
newtype ScopeId = ScopeId Int
  deriving newtype (Eq, Ord, Show)

-- | Control flow context where a binding is used
data ControlContext
  = IfTrueBranch       -- ^ Inside if true branch
  | IfFalseBranch      -- ^ Inside if false branch
  | LoopBody           -- ^ Inside loop body
  | RecursiveCall      -- ^ At recursive call site
  | NormalContext      -- ^ Normal evaluation context
  | ContinuationContext -- ^ Inside call/cc continuation
  deriving (Eq, Ord, Show)

-- | Usage pattern for a binding - tracks where and how it's used
data UsagePattern = UsagePattern
  { bindingId :: BindingId
  , usageLocations :: [Int]           -- ^ Source positions where binding is referenced
  , usageContexts :: [ControlContext] -- ^ Control flow contexts where used
  }
  deriving (Eq, Show)

-- | Scope tree node - represents a lexical scope in the program
data ScopeTreeNode = ScopeTreeNode
  { nodeId :: ScopeId
  , parent :: Maybe ScopeId           -- ^ Parent scope (Nothing for root)
  , children :: [ScopeId]              -- ^ Child scopes
  , scopeBindings :: Set.Set BindingId -- ^ Bindings declared in this scope
  , scopeType :: ScopeType             -- ^ Type of scope (lambda, let, etc.)
  , scopeDepth :: Int                  -- ^ Depth in tree (0 for root)
  }
  deriving (Eq, Show)

-- | Type of scope
data ScopeType
  = TopLevelScope
  | LambdaScope
  | LetScope
  | LetRecScope
  | IfScope
  | LoopScope
  deriving (Eq, Ord, Show)

-- | Scope tree - hierarchical representation of lexical scopes
data ScopeTree = ScopeTree
  { nodes :: Map.Map ScopeId ScopeTreeNode
  , root :: ScopeId
  , nextId :: Int                      -- ^ Next available scope ID
  }
  deriving (Eq, Show)

-- | Enhanced visibility region with scope tree information
data EnhancedVisibilityRegion = EnhancedVisibilityRegion
  { baseRegion :: VisibilityRegion    -- ^ Original position-based region
  , scopeIds :: Set.Set ScopeId       -- ^ Scope tree nodes where binding is visible
  , usagePattern :: UsagePattern      -- ^ Usage pattern information
  }
  deriving (Eq, Show)
