# Troubleshooting Guide

**Common issues and solutions for Computational Scheme Theory**

---

## Table of Contents

1. [Common Errors](#common-errors)
2. [H¹ Computation Issues](#h¹-computation-issues)
3. [Performance Problems](#performance-problems)
4. [Integration Issues](#integration-issues)
5. [FAQ](#faq)

---

## Common Errors

### Error: "unbound identifier"

**Symptom:**
```
unbound identifier: func-access-count
```

**Cause:** Binding order issue in `let` expressions.

**Solution:**
- Change `let` to `let*` for sequential binding
- Ensure all referenced variables are defined before use

**Example Fix:**
```racket
;; Wrong:
(let ([dimension func-access-count]
      [func-access-count (hash-ref map 'x 0)])
  ...)

;; Correct:
(let* ([func-access-count (hash-ref map 'x 0)]
       [dimension func-access-count])
  ...)
```

---

### Error: "hash-ref: contract violation expected: hash?"

**Symptom:**
```
hash-ref: contract violation
  expected: hash?
  given: (list (cons 'α0 (incidence-point ...)))
```

**Cause:** Points stored as list instead of hash table.

**Solution:**
- Check if points is hash or list
- Convert list to hash if needed

**Example Fix:**
```racket
(define points-hash (if (hash? points)
                      points
                      (for/hash ([p-entry points])
                        (values (car p-entry) (cdr p-entry)))))
```

---

### Error: "read-syntax: expected a `)` to close `(`"

**Symptom:**
```
read-syntax: expected a `)` to close `(`
```

**Cause:** Mismatched parentheses in Racket code.

**Solution:**
- Count opening and closing parentheses
- Use editor with parentheses matching
- Check nested function calls

**Example Fix:**
```racket
;; Count parentheses:
;; Opens: ( = 5
;; Closes: ) = 5
;; Must match!
```

---

### Error: Parsing Errors

**Symptom:**
```
Error parsing Scheme source
```

**Cause:** Invalid Scheme syntax in source string.

**Solution:**
- Validate Scheme syntax before calling API
- Check for missing parentheses
- Ensure proper quoting

**Example:**
```racket
;; Invalid:
(define x 10  ; Missing closing paren

;; Valid:
(define x 10)
```

---

## H¹ Computation Issues

### H¹ Always Returns 0

**Symptom:**
- All programs return H¹ = 0, even recursive ones

**Possible Causes:**

1. **Missing dimensional enhancement**
   - Check if `compute-h1-incidence` uses dimensional weighting
   - Verify access counting is working

2. **Incomplete cycle detection**
   - Ensure recursive calls are detected
   - Check that cycle intermediates are created

3. **Missing projective points**
   - Optional bindings may need projective closure
   - Check for undefined/branch paths

**Solution:**

```racket
;; Verify dimensional framework is active:
(let-values ([(h1-inc error) (compute-h1-from-source source)])
  ;; Check if incidence structure has dimensional info
  ;; Check if recursive functions have dimension >= 1
  )
```

**See Also:**
- [Why H¹ is Zero](docs/00%20-%20INBOX/update/files%20(4)/WHY_H1_IS_ZERO.md)
- [Dimensional Framework Integration](docs/00%20-%20INBOX/update/files%20(4)/DIMENSIONAL_FRAMEWORK_INTEGRATION.md)

---

### H¹ Returns Negative Values

**Symptom:**
```
H¹ = -1
```

**Cause:** Incorrect kernel/image computation.

**Solution:**
- Verify `dim(Ker(d₁)) >= dim(Im(d₂))`
- Check matrix rank calculations
- Ensure proper boundary map construction

**Example Fix:**
```racket
(define base-h1 (max 0 (- ker-d1-dim im-d2-dim)))  ; Use max to avoid negatives
```

---

### Inconsistent H¹ Values

**Symptom:**
- Same program gives different H¹ values on different runs

**Cause:** Non-deterministic behavior in computation.

**Solution:**
- Check for hash iteration order dependencies
- Ensure deterministic ordering of bindings
- Use sorted lists for consistent results

---

## Performance Problems

### Slow H¹ Computation

**Symptom:**
- Large programs take very long to compute

**Possible Causes:**

1. **Large number of bindings**
   - Matrix operations scale as O(m²) with m bindings

2. **Complex dependency graphs**
   - Many edges increase computation time

**Solutions:**

1. **Optimize matrix operations**
   - Use sparse matrix representation
   - Consider iterative solvers for large systems

2. **Limit computation scope**
   - Analyze only relevant parts of program
   - Skip unused bindings

3. **Cache results**
   - Store computed H¹ for repeated analysis
   - Use memoization where appropriate

---

### Memory Issues

**Symptom:**
- Out of memory errors on large programs

**Cause:** Large incidence matrices consuming memory.

**Solution:**
- Use sparse matrix structures
- Process programs in chunks
- Increase system memory limits

---

## Integration Issues

### Service Bridge Not Available

**Symptom:**
```
racket-service-available? returns #f
```

**Cause:** Service not running or misconfigured.

**Solution:**

1. **Check service URL:**
   ```racket
   (printf "Service URL: ~a\n" *racket-service-url*)
   ```

2. **Verify service is running:**
   - Check service process
   - Verify network connectivity
   - Check firewall settings

3. **Use direct computation:**
   ```racket
   ;; Fall back to direct computation if service unavailable
   (if (racket-service-available?)
       (call-racket-vg source)
       (compute-vg-direct source))  ; Use direct computation
   ```

---

### Python Coordinator Errors

**Symptom:**
- Python coordinator fails to communicate with Racket

**Solution:**

1. **Check Racket process:**
   ```bash
   ps aux | grep racket
   ```

2. **Verify paths:**
   - Ensure Python can find Racket executable
   - Check environment variables

3. **Test direct computation:**
   ```python
   from coordinator.direct_compute import DirectComputeCoordinator
   coordinator = DirectComputeCoordinator()
   result = coordinator.validate_program("test", program)
   ```

---

## FAQ

### Q: Why does my recursive function return H¹ = 0?

**A:** Check that:
1. Dimensional framework is enabled
2. Recursive calls are detected
3. Cycle intermediates are created
4. Access counting is working

**See:** [Dimensional Framework Status](DIMENSIONAL_FRAMEWORK_STATUS.md)

---

### Q: What's the difference between H¹ (Čech) and H¹ (Incidence)?

**A:**
- **H¹ (Čech)**: Computed from Čech complex (topology-based)
- **H¹ (Incidence)**: Computed from incidence structure (recommended)

**Recommendation:** Use H¹ (Incidence) - it's more accurate with dimensional framework.

---

### Q: How do I interpret H¹ values?

**A:**
- **H¹ = 0**: No cycles detected (tree-like structure)
- **H¹ = 1**: One independent cycle (e.g., recursive function)
- **H¹ > 1**: Multiple independent cycles

**See:** [H¹ Computation Guide](docs/00%20-%20INBOX/update/files%20(4)/H1_COMPUTATION_GUIDE.md)

---

### Q: Can I compute H¹ for partial programs?

**A:** Yes, but ensure:
- Valid Scheme syntax (even if incomplete)
- Proper error handling for undefined bindings
- Consider using projective types for optional parts

---

### Q: How do I debug dimensional framework issues?

**A:**

1. **Check access counts:**
   ```racket
   ;; Print access counts for each binding
   (for ([(name count) access-map])
     (printf "~a: ~a accesses\n" name count))
   ```

2. **Verify dimensions:**
   ```racket
   ;; Check point dimensions
   (for ([(id point) points])
     (printf "~a: ~aD (Church ~a)\n" 
             id 
             (incidence-point-dimension point)
             (incidence-point-dimension point)))
   ```

3. **Enable verbose output:**
   - Check test files for debug output patterns
   - Use demo framework for visualization

---

### Q: What if my program uses complex patterns?

**A:**
- Pattern matching with ellipsis `...` is fully supported
- Complex nesting should work
- Check pattern matching interpretation in output

**See:** [Pattern Matching Framework](docs/00%20-%20INBOX/update/files%20(4)/The%20Pattern%20Matching%20IS%20the%20Complete%20Framework.md)

---

### Q: How do I handle errors gracefully?

**A:**

```racket
(let-values ([(h1 error) (compute-h1-from-source source)])
  (if error
      (begin
        (printf "Error computing H¹: ~a\n" error)
        ;; Handle error appropriately
        #f)
      (begin
        ;; Use h1 value
        h1)))
```

---

### Q: Can I use this with other languages?

**A:**
- Current implementation focuses on Scheme
- Architecture allows extension to other languages
- Would require parser for target language

**See:** [System Architecture](racket-unified/ARCHITECTURE.md)

---

## Getting Help

### Debugging Steps

1. **Check error messages** - They often indicate the problem
2. **Verify input** - Ensure valid Scheme syntax
3. **Test with simple programs** - Start with minimal examples
4. **Enable verbose output** - Use demo framework
5. **Review documentation** - Check relevant guides

### Useful Debugging Tools

- `demo-dimensional-framework.rkt` - Interactive analysis
- Test files in `racket-unified/test/` - Reference implementations
- Python examples - `docs/00 - INBOX/update/files (4)/h1_incidence_computation.py`

### Logging

Enable detailed logging by:
```racket
;; Set debug mode if available
(current-logger debug-logger)
```

---

## Performance Tips

1. **Cache results** for repeated analysis
2. **Use direct computation** instead of service bridge when possible
3. **Limit scope** of analysis for large programs
4. **Profile** to identify bottlenecks

---

## See Also

- [API Reference](API_REFERENCE.md) - Function documentation
- [User Guide](docs/USING_DIMENSIONAL_FRAMEWORK.md) - Usage examples
- [System Complete](SYSTEM_COMPLETE.md) - System overview
- [Why H¹ is Zero](docs/00%20-%20INBOX/update/files%20(4)/WHY_H1_IS_ZERO.md) - Analysis

---

*Last Updated: 2025-01-31*

