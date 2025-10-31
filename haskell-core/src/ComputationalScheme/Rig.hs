{-# LANGUAGE NoImplicitPrelude #-}

-- | R_Scheme rig operations and utilities
--
-- This module provides additional operations on the R_Scheme rig beyond
-- the basic Semiring instance.
module ComputationalScheme.Rig where

import ComputationalScheme.Types (RScheme(..), BindingId)
import qualified Data.Set as Set
import Prelude

-- | Check if a binding is in scope
bindingInScope :: BindingId -> RScheme -> Bool
bindingInScope bid (RScheme bindings) = Set.member bid bindings

-- | Add a binding to the rig
addBinding :: BindingId -> RScheme -> RScheme
addBinding bid (RScheme bindings) = RScheme (Set.insert bid bindings)

-- | Remove a binding from the rig
removeBinding :: BindingId -> RScheme -> RScheme
removeBinding bid (RScheme bindings) = RScheme (Set.delete bid bindings)

-- | Number of bindings in the rig
bindingCount :: RScheme -> Int
bindingCount (RScheme bindings) = Set.size bindings

-- | Check if rig is empty
isEmpty :: RScheme -> Bool
isEmpty (RScheme bindings) = Set.null bindings

-- | Get all bindings as a list (for iteration)
allBindings :: RScheme -> [BindingId]
allBindings (RScheme bindings) = Set.toList bindings

