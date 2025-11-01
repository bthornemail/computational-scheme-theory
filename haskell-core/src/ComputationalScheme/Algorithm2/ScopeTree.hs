{-# LANGUAGE OverloadedStrings #-}

-- | Scope Tree Construction and Tree-Based Overlap Detection
--
-- Implements the alternative scope-tree approach for overlap detection.
-- Uses explicit parent-child relationships instead of position ranges.
module ComputationalScheme.Algorithm2.ScopeTree where

import ComputationalScheme.Types
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Debug.Trace (trace)

-- | Check if scope s1 is an ancestor of scope s2 (including self)
isAncestor :: ScopeTree -> ScopeId -> ScopeId -> Bool
isAncestor tree s1 s2
  | s1 == s2 = True
  | otherwise = case Map.lookup s2 (nodes tree) of
      Just node -> case parent node of
        Just p -> isAncestor tree s1 p
        Nothing -> False
      Nothing -> False

-- | Check if scope s1 is a descendant of scope s2 (including self)
isDescendant :: ScopeTree -> ScopeId -> ScopeId -> Bool
isDescendant tree s1 s2 = isAncestor tree s2 s1

-- | Check if two scopes overlap using tree structure
-- Two scopes overlap if one is ancestor/descendant of the other
scopeOverlaps :: ScopeTree -> ScopeId -> ScopeId -> Bool
scopeOverlaps tree s1 s2 = 
  isAncestor tree s1 s2 || isAncestor tree s2 s1

-- | Get all ancestors of a scope (including self)
getAncestors :: ScopeTree -> ScopeId -> [ScopeId]
getAncestors tree scopeId = 
  case Map.lookup scopeId (nodes tree) of
    Just node -> case parent node of
      Just p -> scopeId : getAncestors tree p
      Nothing -> [scopeId]
    Nothing -> [scopeId]

-- | Get all descendants of a scope (including self)
getDescendants :: ScopeTree -> ScopeId -> Set.Set ScopeId
getDescendants tree scopeId =
  case Map.lookup scopeId (nodes tree) of
    Just node -> 
      let childSets = map (getDescendants tree) (children node)
      in Set.unions (Set.singleton scopeId : childSets)
    Nothing -> Set.singleton scopeId

-- | Check if two visibility regions overlap based on scope tree
-- Two regions overlap if they share any scope tree nodes,
-- or if one scope is ancestor/descendant of another
treeBasedOverlap :: ScopeTree -> EnhancedVisibilityRegion -> EnhancedVisibilityRegion -> Bool
treeBasedOverlap tree (EnhancedVisibilityRegion _ scopeIds1 _) (EnhancedVisibilityRegion _ scopeIds2 _) =
  -- Check if any scope in region 1 overlaps with any scope in region 2
  any (\s1 -> any (\s2 -> scopeOverlaps tree s1 s2) (Set.toList scopeIds2)) (Set.toList scopeIds1)

-- | Find all pairs of overlapping scopes in the tree
-- Returns pairs (scope1, scope2) where scope1 is ancestor of scope2
findOverlappingPairs :: ScopeTree -> [(ScopeId, ScopeId)]
findOverlappingPairs tree =
  let allScopes = Map.keys (nodes tree)
      -- For each pair, check if one is ancestor of the other
      pairs = [(s1, s2) | s1 <- allScopes, s2 <- allScopes, s1 < s2, scopeOverlaps tree s1 s2]
  in trace ("[ScopeTree] Found " ++ show (length pairs) ++ " overlapping scope pairs") pairs

-- | Get depth of a scope in the tree
getScopeDepth :: ScopeTree -> ScopeId -> Int
getScopeDepth tree scopeId =
  case Map.lookup scopeId (nodes tree) of
    Just node -> scopeDepth node
    Nothing -> 0

-- | Find least common ancestor of two scopes
findLCA :: ScopeTree -> ScopeId -> ScopeId -> Maybe ScopeId
findLCA tree s1 s2 =
  let ancestors1 = Set.fromList (getAncestors tree s1)
      ancestors2 = getAncestors tree s2
      -- Find first ancestor of s2 that is also ancestor of s1
      common = filter (`Set.member` ancestors1) ancestors2
  in case common of
    [] -> Nothing
    (x:_) -> Just x

