# Hypothesis Analysis: H¹ = V(G) - k

**Date**: 2025-01-31  
**Sample Size**: 4 programs  
**Status**: ⚠️ Preliminary findings - hypothesis does not hold in expected form

## Sample Data

| Program | Category | H¹ | V(G) | k = V(G) - H¹ |
|---------|----------|----|------|---------------|
| B001.scm | baseline | 0 | 10 | 10 |
| CC001.scm | complex-control | 0 | 47 | 47 |
| SC001.scm | simple-control | 0 | 19 | 19 |
| R001.scm | recursion | 0 | 29 | 29 |

## Key Finding

**k is NOT constant** - it varies directly with V(G):

```
k = V(G) - H¹ = V(G) - 0 = V(G)
```

Since V(G) varies from 10 to 47, k also varies.

## Hypothesis Testing

### Original Hypothesis
```
H¹ = V(G) - k  (where k is a constant)
```

### Observed Relationship
```
H¹ = 0
k = V(G)
```

### Implication

**The original hypothesis does NOT hold** as stated. When H¹ = 0:
- k is not a constant
- k equals V(G)
- The relationship is: `0 = V(G) - V(G)` (trivially true)

## Possible Interpretations

### Interpretation 1: Modified Hypothesis

The relationship might be:
```
H¹ = max(0, V(G) - k)
```

Where k ≥ max(V(G)) for programs analyzed. This would yield H¹ = 0 for all current programs.

**Test Needed**: Find programs where V(G) > k to see if H¹ > 0.

### Interpretation 2: Different Measures

Scope topology (H¹) and control flow complexity (V(G)) may be **fundamentally different measures**:
- **H¹**: Measures cycles/holes in static scope topology
- **V(G)**: Measures cycles/paths in dynamic control flow

They may not directly correlate.

### Interpretation 3: Complete Graph Problem

**Observation**: All programs produce complete overlap graphs (all bindings overlap), resulting in H¹ = 0.

**Question**: Does the scope topology inherently create complete graphs for typical programs?

**Hypothesis**: The scope assignment strategy may need modification to create non-trivial topology structures.

## Statistical Analysis (Preliminary)

**Correlation**:
- H¹ values: All 0 (no variation)
- V(G) values: 10, 19, 29, 47 (high variation)
- Correlation cannot be computed (H¹ has zero variance)

**k Distribution**:
- Mean: 26.25
- Range: 10 to 47
- Standard deviation: 15.84
- **Conclusion**: k is highly variable, not constant

## Next Steps

### 1. Expand Sample Size

Test more programs to:
- Confirm k variability
- Check if any programs have H¹ > 0
- Identify patterns in k values

### 2. Investigate Scope Topology

**Question**: Why do all programs produce H¹ = 0?

**Possible causes**:
- Scope assignment creates complete graphs
- All bindings overlap (no gaps)
- Need different scope modeling approach

**Action**: 
- Analyze scope tree structures
- Check if scope assignment can be modified
- Test programs designed to create non-complete graphs

### 3. Reformulate Hypothesis

Based on findings, consider:
- `H¹ = f(V(G))` where f is not linear with constant k
- `H¹ = max(0, V(G) - k(V(G)))` where k depends on V(G)
- Scope topology and CFG complexity are independent measures

### 4. Theoretical Investigation

**Research Questions**:
1. What should H¹ measure in the context of program complexity?
2. Can scope topology be modified to create non-trivial H¹ values?
3. Is there a relationship between scope cycles and control flow cycles?

## Conclusion

**Preliminary Finding**: The hypothesis `H¹ = V(G) - k` (with constant k) does not hold for the sample.

**Observation**: When H¹ = 0, k = V(G), indicating k is not constant.

**Recommendation**: 
1. Investigate why all programs have H¹ = 0
2. Test hypothesis with programs that produce H¹ > 0
3. Consider reformulating the hypothesis based on empirical findings

## Files

- `sample_validation.json`: Sample validation results
- `h1_values.json`: Full H¹ collection (64 programs, all H¹ = 0)
- `validate_sample.py`: Sample validation script

