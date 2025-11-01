# H¹ Zero Issue - Implementation Summary

## Problem Identified
H¹ remains 0 because no 1-simplices (edges) are created in the Čech complex.
- This happens when visibility regions D(f) don't overlap
- Even with nested scopes, if regions don't overlap, β₁ = 0

## Fixes Implemented

### ✅ 1. Scope Region Tracking (Scope.hs)
- **Changed**: Added active scope region tracking to state monad
- **Before**: Each binding got isolated region `ScopeRegion start (start+100)`
- **After**: Nested bindings accumulate regions from outer scopes
- **Impact**: Bindings in nested scopes now share multiple regions

### ✅ 2. Range Overlap Detection (Nerve.hs & OpenCover.hs)
- **Changed**: Fixed `hasIntersection` to check position range overlaps
- **Before**: Used `Set.intersection` which only finds identical `ScopeRegion` objects
- **After**: Checks if position ranges `[start, end]` actually overlap
- **Formula**: `max(start1, start2) <= min(end1, end2)`
- **Impact**: Correctly detects when regions overlap spatially

### ⚠️ 3. Known Issue: Source Positions
- **Problem**: Parser sets all `locPos` to 0
- **Impact**: All regions start at position 0, but ends differ by depth
- **Status**: Should still work if regions have different end positions

## Testing Results
- Programs still return H¹ = 0
- V(G) values are correct (non-zero: 20-32)
- Hypothesis not holding: H¹ ≠ V(G) - k

## Next Investigation Needed
1. Verify scope regions are actually being accumulated correctly
2. Check if region end positions differ enough to overlap
3. May need alternative approach: use scope depth/nesting instead of positions
