# Scope Accumulation Fix

**Date**: 2025-01-31  
**Status**: ✅ Fixed - Lexical Scoping Implementation

## Problem Identified

**Root Cause**: Scope accumulation mechanism in `ScopeAnalysis.hs`

The previous implementation (lines 245-255) added the **current scope** to a binding's visibility **every time** it was referenced:

```haskell
-- OLD: When a binding is referenced, add current scope
currentScopeId <- gets currentScope
case currentScopeId of
  Just scopeId -> do
    let updatedScopeIds = Set.insert scopeId (scopeIds region)
```

**Problem**: This created **complete overlap graphs** because:
- Every binding that was referenced in nested scopes accumulated ALL those scopes
- All bindings ended up overlapping with each other
- Result: H¹ = 0 for all programs

## Solution

**Fix**: Compute visibility based on **lexical scoping rules** only:
1. Binding is visible in its **definition scope**
2. Binding is visible in **all descendant scopes** (children, grandchildren, etc.)
3. Visibility is computed **after** the scope tree is complete, not during traversal

### Implementation

**Removed**: Scope accumulation on variable reference (Var case)

**Added**: `computeFinalVisibility` function that:
- Takes the complete scope tree and binding map
- For each binding, expands visibility to include all descendant scopes
- Uses `getDescendants` from `ScopeTree.hs` to get all descendant scopes

```haskell
computeFinalVisibility :: ScopeTree -> Map.Map BindingId EnhancedVisibilityRegion -> Map.Map BindingId EnhancedVisibilityRegion
computeFinalVisibility tree bindingMap =
  Map.mapWithKey (\_bindingId region -> 
    let defScopeIds = scopeIds region
        -- For each definition scope, include the scope itself and all its descendants
        allVisibleScopes = Set.unions $ map (\defScope -> getDescendants tree defScope) (Set.toList defScopeIds)
    in region { scopeIds = allVisibleScopes }
  ) bindingMap
```

**Updated**: `analyzeScopesEnhanced` to call `computeFinalVisibility` after scope tree is built:

```haskell
analyzeScopesEnhanced expr = 
  let (_, finalState) = runState (analyzeScopesExprEnhanced expr) initialState
      finalTree = scopeTree finalState
      -- Compute final visibility: bindings visible in definition scope + all descendant scopes
      finalBindings = computeFinalVisibility finalTree (bindingMap finalState)
  in (finalBindings, finalTree)
```

## Impact

### Before Fix
- **CC001.scm**: 4 vertices, **6 edges** (complete graph K₄), 4 triangles, H¹ = 0
- All bindings overlapped with each other

### After Fix (Expected)
- **CC001.scm**: 4 vertices, **3 edges** (star graph), 0 triangles, H¹ = 0
- Function overlaps with parameters, but parameters don't overlap with each other
- More realistic scope topology

## Testing

Initial test results show:
- CC001.scm: Now shows 3 edges instead of 6 (star graph instead of complete graph)
- Topology is more realistic

### Next Steps
1. Test full corpus to see if any programs produce H¹ > 0
2. Analyze if the new topology structure better correlates with V(G)
3. Document any programs that produce non-zero H¹

## Files Modified

- `haskell-core/src/ComputationalScheme/Algorithm2/ScopeAnalysis.hs`:
  - Removed scope accumulation in `Var` case (lines 230-262)
  - Added `computeFinalVisibility` function (lines 321-331)
  - Updated `analyzeScopesEnhanced` to call final visibility computation (lines 73-77)

## Lexical Scoping Rules

The fix implements proper **lexical scoping**:
- A binding is visible in the scope where it's **defined**
- A binding is visible in all **nested scopes** (descendants)
- A binding is **NOT** visible in parent or sibling scopes

This is the standard lexical scoping model for Scheme/R5RS.

