# Known Issues and Limitations

## Overview

This document catalogs known issues, limitations, and workarounds for the Combinator Algebra Extension implementation.

**Last Updated**: [Current Date]
**Issue Tracking**: Manual (this document)

## Critical Issues

### 1. Consensus Protocol Simplification

**Issue ID**: CA-001  
**Severity**: High  
**Status**: Known Limitation  
**Priority**: High

**Description**:
Consensus protocols (`z-field-consensus` and `y-ring-consensus`) are implemented in a simplified form that does not perform actual consensus computation. They return combinator procedures rather than computing actual agreement states.

**Symptoms**:
- Consensus functions return procedures instead of consensus states
- Iterations always reported as 1
- No actual convergence computation visible
- No multi-node agreement simulation

**Impact**:
- High: Core functionality not fully implemented
- Consensus protocols cannot be used for distributed agreement
- Compliance with Appendix Z Section 4.1 is partial

**Root Cause**:
Implementation simplified for initial version. Full distributed consensus algorithms not yet implemented.

**Workaround**:
None - functionality not available.

**Recommendation**:
1. Implement proper distributed consensus algorithms
2. Add multi-node simulation
3. Verify convergence properties
4. Add comprehensive consensus tests

**Related Files**:
- `racket-unified/src/algorithms/combinator-algebra.rkt` (lines 222-274)

### 2. Fixed-Point Return Values

**Issue ID**: CA-002  
**Severity**: Medium  
**Status**: Design Question  
**Priority**: Medium

**Description**:
Fixed-point operations (`fixed-point-algebra`, `find-fixed-point`) return combinator procedures rather than computed fixed-point values.

**Symptoms**:
- `fixed-point-algebra` returns Y-combinator procedure
- `find-fixed-point` returns Z-combinator procedure
- Values not directly usable without additional application

**Impact**:
- Medium: Usage pattern unclear
- May require additional function application
- Documentation needs clarification

**Root Cause**:
Unclear whether procedures or values should be returned. Current implementation returns procedures.

**Workaround**:
Apply returned procedures to get actual values:
```racket
(define fp (fixed-point-algebra "TestRing" f))
(define value (fp arg))
```

**Recommendation**:
1. Clarify expected behavior in specification
2. Update implementation if needed
3. Add examples to documentation
4. Add test cases with concrete values

**Related Files**:
- `racket-unified/src/algorithms/combinator-algebra.rkt` (lines 97-113, 154-169)

## Moderate Issues

### 3. Event Log Serialization

**Issue ID**: CA-003  
**Severity**: Low  
**Status**: Known Limitation  
**Priority**: Low

**Description**:
Event log file contains non-serializable data (procedures), causing read errors when loading events from file.

**Symptoms**:
- Error: `data/events.log::173: read: bad syntax \`#<\``
- Event log cannot be fully reloaded
- Procedures written to log file cannot be read back

**Impact**:
- Low: Error handling works gracefully
- Tests continue with in-memory event store
- Event replay not fully functional

**Root Cause**:
Procedures (functions) are not serializable in Racket's default read/write mechanism. Event log attempts to write procedures.

**Workaround**:
1. Clear event log file
2. Continue with in-memory event store only
3. Handle errors gracefully (as tests do)

**Recommendation**:
1. Implement proper event serialization
2. Exclude procedures from event log
3. Use event IDs instead of procedures
4. Add event validation

**Related Files**:
- `racket-unified/src/s-expression.rkt` (lines 44-51)
- `racket-unified/src/persistence/event-store-file.rkt`

### 4. Test Suite Incompleteness

**Issue ID**: CA-004  
**Severity**: Medium  
**Status**: In Progress  
**Priority**: Medium

**Description**:
Test suite is incomplete. Only ~10% of planned tests (90 total) are implemented.

**Symptoms**:
- Basic tests exist but expanded tests not complete
- Missing test categories:
  - Mutual recursion (15 tests)
  - Distributed consensus (25 tests)
  - Complex structures (20 tests)
  - Real-world protocols (10 tests)

**Impact**:
- Medium: Limited test coverage
- Compliance with Appendix Z Section 6.2 is partial
- Edge cases not fully tested

**Root Cause**:
Initial implementation focused on basic functionality. Comprehensive test suite not yet completed.

**Workaround**:
None - tests need to be implemented.

**Recommendation**:
1. Implement all planned test categories
2. Add error handling tests
3. Add boundary condition tests
4. Add performance tests

**Related Files**:
- `racket-unified/test-combinator-algebra.rkt`
- `racket-unified/test-combinator-algebra-expanded.rkt`

### 5. Resource Limits Not Tested

**Issue ID**: CA-005  
**Severity**: Medium  
**Status**: Not Tested  
**Priority**: Medium

**Description**:
Resource limits specified in Appendix Z are not tested:
- Maximum recursion depth: 1,000,000
- Maximum iterations: 10,000
- Timeout limit: 30 seconds

**Symptoms**:
- No tests for deep recursion scenarios
- No tests for maximum iteration bounds
- No tests for timeout behavior
- Unknown behavior at limits

**Impact**:
- Medium: Compliance verification incomplete
- Unknown reliability at limits
- Potential issues in production

**Root Cause**:
Resource limit tests not yet implemented.

**Workaround**:
None - tests need to be implemented.

**Recommendation**:
1. Add recursion depth limit tests
2. Add iteration bound tests
3. Add timeout tests
4. Verify resource limit enforcement

**Related Files**:
- `racket-unified/src/algorithms/combinator-algebra.rkt`
- Test files (to be created)

## Minor Issues

### 6. Registry Cleanup

**Issue ID**: CA-006  
**Severity**: Low  
**Status**: Enhancement  
**Priority**: Low

**Description**:
Registry entries persist between test runs. No cleanup mechanism provided.

**Symptoms**:
- Registry accumulates entries
- Tests may interfere with each other
- Manual cleanup needed for isolation

**Impact**:
- Low: Tests work but may have side effects
- Test isolation not perfect

**Root Cause**:
No registry cleanup functionality implemented.

**Workaround**:
Manually clear registry if needed:
```racket
(hash-clear! y-ring-registry)
(hash-clear! z-field-registry)
```

**Recommendation**:
1. Add registry cleanup functions
2. Optionally clear registry between tests
3. Document registry persistence behavior

**Related Files**:
- `racket-unified/src/algorithms/combinator-algebra.rkt` (lines 19, 30)

### 7. Documentation Gaps

**Issue ID**: CA-007  
**Severity**: Low  
**Status**: Documentation  
**Priority**: Low

**Description**:
Some implementation details are not fully documented:
- Fixed-point return behavior
- Consensus protocol limitations
- Resource limits
- Error handling

**Symptoms**:
- Unclear usage patterns
- Missing examples
- Incomplete API documentation

**Impact**:
- Low: Functionality works but unclear
- Developer confusion possible

**Root Cause**:
Documentation not yet complete.

**Workaround**:
Read source code for details.

**Recommendation**:
1. Complete API documentation
2. Add usage examples
3. Document limitations
4. Add troubleshooting guide

**Related Files**:
- Documentation files
- `COMBINATOR_ALGEBRA_IMPLEMENTATION.md`

## Design Limitations

### 8. Simplified Consensus

**Issue ID**: CA-008  
**Severity**: Design  
**Status**: Design Decision  
**Priority**: Medium

**Description**:
Consensus protocols are simplified and do not implement full distributed consensus algorithms.

**Limitations**:
- No actual agreement computation
- No network simulation
- No failure handling
- No convergence guarantees

**Impact**:
- Medium: Cannot be used for real distributed systems
- Compliance with Appendix Z Section 4.1 is partial

**Design Rationale**:
Simplified for initial implementation. Full distributed consensus requires additional infrastructure.

**Recommendation**:
1. Implement proper consensus algorithms (Raft, PBFT, etc.)
2. Add network simulation layer
3. Implement failure handling
4. Verify convergence properties

### 9. Single-Threaded Execution

**Issue ID**: CA-009  
**Severity**: Design  
**Status**: Design Decision  
**Priority**: Low

**Description**:
Implementation is single-threaded. No concurrent access support.

**Limitations**:
- No thread safety
- No concurrent operations
- No distributed execution

**Impact**:
- Low: Current use cases don't require concurrency
- May limit scalability

**Design Rationale**:
Simplified for initial implementation. Concurrency adds complexity.

**Recommendation**:
1. Add thread safety if needed
2. Implement concurrent access patterns
3. Add distributed execution support

## Workarounds Summary

### For Issue CA-001 (Consensus Simplification)
- **Workaround**: None - functionality not available
- **Impact**: Cannot use consensus protocols

### For Issue CA-002 (Fixed-Point Returns)
- **Workaround**: Apply returned procedures to get values
- **Impact**: Additional function application needed

### For Issue CA-003 (Event Log Serialization)
- **Workaround**: Clear event log or use in-memory store only
- **Impact**: Event replay not fully functional

### For Issue CA-004 (Test Suite Incompleteness)
- **Workaround**: None - tests need to be implemented
- **Impact**: Limited test coverage

### For Issue CA-005 (Resource Limits Not Tested)
- **Workaround**: None - tests need to be implemented
- **Impact**: Unknown behavior at limits

### For Issue CA-006 (Registry Cleanup)
- **Workaround**: Manually clear registry if needed
- **Impact**: Test isolation not perfect

### For Issue CA-007 (Documentation Gaps)
- **Workaround**: Read source code for details
- **Impact**: Developer confusion possible

## Issue Resolution Plan

### Immediate (High Priority)

1. **CA-001**: Implement proper consensus protocols
2. **CA-002**: Clarify fixed-point return behavior

### Short-term (Medium Priority)

3. **CA-004**: Complete test suite implementation
4. **CA-005**: Add resource limit tests
5. **CA-008**: Design consensus protocol enhancement

### Long-term (Low Priority)

6. **CA-003**: Fix event log serialization
7. **CA-006**: Add registry cleanup
8. **CA-007**: Complete documentation
9. **CA-009**: Consider concurrency support

## Reporting Issues

### Issue Reporting Format

When reporting new issues, include:

1. **Issue ID**: CA-XXX (auto-assigned)
2. **Severity**: Critical / High / Medium / Low
3. **Description**: Clear description of issue
4. **Symptoms**: Observable behavior
5. **Steps to Reproduce**: How to trigger issue
6. **Expected Behavior**: What should happen
7. **Actual Behavior**: What actually happens
8. **Environment**: System details
9. **Related Files**: Code locations

### Issue Tracking

- **Location**: This document
- **Format**: Manual entries
- **Status**: Active, Resolved, Deferred

## Conclusion

The Combinator Algebra Extension implementation is functional but has several known limitations and areas for improvement. Most issues are non-critical and can be addressed incrementally.

**Overall Status**: ✅ **FUNCTIONAL** (with known limitations)

**Recommendation**: Address high-priority issues (CA-001, CA-002) first, then proceed with medium-priority improvements.
