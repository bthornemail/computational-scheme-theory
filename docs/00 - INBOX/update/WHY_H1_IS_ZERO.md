# Why H¹ is Still Zero: Analysis and Solution

## The Problem

Our Scheme → H¹ pipeline is working correctly, but **all examples show H¹ = 0**. This actually validates your research question:

> "Why is H¹ mostly zero?"

The answer: **We're not capturing the full cyclic structure yet!**

## What's Missing

### 1. Recursive Call Dependencies

**Current behavior:**
```scheme
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

**What we capture:**
- Bindings: `factorial`, `n`, various operators
- Constraints: `application`, `branch_convergence`

**What we MISS:**
- The dependency: `factorial` depends on `factorial` (recursive call!)
- This creates a CYCLE: `factorial → factorial`

**Fix:**  
Need to track **variable usage** and create dependencies:
```python
if isinstance(func, Var) and func.name in defined_functions:
    # This is a recursive call!
    self.add_dependency(current_function, func.name)
    if current_function == func.name:
        # Self-recursion creates cycle
        self.add_constraint("self_recursion", scope)
```

### 2. Y-Combinator Fixed Point Structure

**Current behavior:**
```scheme
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))
```

**What we detect:**
- Pattern matching recognizes Y-combinator
- Adds `y_combinator` and `fixed_point` constraints

**What we MISS:**
- The self-application `(x x)` creates a cycle
- The fixed point equation: `Y f = f (Y f)` is a cycle!

**Fix:**
Need to add CYCLE EDGE when Y-combinator is detected:
```python
if self.is_y_combinator(expr):
    # Add explicit cycle through fixed point
    fixed_point_name = f"fixpoint_{scope}"
    self.add_binding(fixed_point_name, scope, "affine")
    self.add_constraint("y_fixed_point", scope)
    
    # CRITICAL: Create self-loop
    self.add_incidence(fixed_point_name, "y_fixed_point")
    self.add_incidence(fixed_point_name, "y_fixed_point")  # Loop!
```

### 3. Projective Closure Connectivity

**Current behavior:**
```scheme
(define (safe-divide x y)
  (if (= y 0)
      'undefined
      (/ x y)))
```

**What we capture:**
- Projective point: `'undefined` with type `projective`
- Constraint: `projective_closure`

**What we MISS:**
- Connection between branches through projective closure
- The cycle: `test → undefined(∞) → closure → result → test`

**Fix:**
Need to connect projective points to closure:
```python
if isinstance(expr.else_branch, Undefined):
    undefined_name = f"undefined_{if_scope}"
    self.add_binding(undefined_name, if_scope, "projective")
    
    # Add projective closure constraint
    closure_constraint = f"projective_closure_{if_scope}"
    self.add_constraint(closure_constraint, if_scope)
    
    # CRITICAL: Connect undefined to closure
    self.add_incidence(undefined_name, closure_constraint)
    
    # Connect then branch result to closure too
    result_name = f"result_{if_scope}"
    self.add_binding(result_name, if_scope, "affine")
    self.add_incidence(result_name, closure_constraint)
    
    # This creates cycle: test → undefined → closure → result → back
```

## The Core Issue: Tree vs Graph

**Current incidence structure:**
```
Points → Constraints
   ↓         ↓
  TREE    (no cycles)
```

**What we need:**
```
Points ↔ Constraints
   ↓↗        ↓↖
  GRAPH  (with cycles!)
```

The incidence matrix is **too sparse**. We're only connecting points to constraints in the same scope, but we need to:

1. **Track variable dependencies across scopes**
2. **Create explicit cycle edges for recursion**
3. **Connect projective closures to create cycles**

## Proposed Fix: Enhanced Datalog Generation

### Fix 1: Track Function Context

```python
class DatalogGenerator:
    def __init__(self):
        # ... existing ...
        self.current_function = None  # Track which function we're in
        self.function_calls = []  # Track all function calls
```

### Fix 2: Detect Recursive Calls

```python
def generate(self, expr: MExpr, scope: str = None):
    # ... existing ...
    
    if isinstance(expr, Define):
        # Remember we're defining this function
        old_function = self.current_function
        self.current_function = expr.name
        
        self.generate(expr.value, define_scope)
        
        self.current_function = old_function
    
    elif isinstance(expr, Apply):
        if isinstance(expr.func, Var):
            func_name = expr.func.name
            
            # Check if this is a recursive call
            if func_name == self.current_function:
                # SELF-RECURSION - creates cycle!
                self.add_constraint("recursion_point", app_scope)
                self.add_incidence(func_name, "recursion_point")
                
                # Create cycle edge
                cycle_name = f"cycle_{func_name}"
                self.add_binding(cycle_name, app_scope, "affine")
                self.add_incidence(cycle_name, "recursion_point")
                self.add_incidence(self.current_function, "recursion_point")
```

### Fix 3: Enhanced Y-Combinator Handling

```python
def generate(self, expr: MExpr, scope: str = None):
    # ... existing ...
    
    if isinstance(expr, Lambda):
        lambda_scope = self.fresh_scope()
        
        if self.is_y_combinator(expr):
            # Y-combinator creates fixed point
            self.add_constraint("y_fixed_point", lambda_scope)
            
            # CRITICAL: Create cycle structure
            # Y f = f (Y f) means Y depends on itself
            fp_point = f"fixed_point_{lambda_scope}"
            self.add_binding(fp_point, lambda_scope, "affine")
            
            # Create self-loop in incidence
            self.add_incidence(fp_point, "y_fixed_point")
            
            # Also connect to parameters to show dependency
            for param in expr.params:
                self.add_incidence(param, "y_fixed_point")
```

### Fix 4: Projective Closure Cycles

```python
def generate(self, expr: MExpr, scope: str = None):
    # ... existing ...
    
    if isinstance(expr, If):
        if_scope = self.fresh_scope()
        
        # Track both branches
        then_result = f"then_{if_scope}"
        else_result = f"else_{if_scope}"
        
        self.generate(expr.test, if_scope)
        
        if isinstance(expr.else_branch, Undefined):
            # Projective branch
            self.add_binding(else_result, if_scope, "projective")
        else:
            self.add_binding(else_result, if_scope, "affine")
            self.generate(expr.else_branch, if_scope)
        
        self.add_binding(then_result, if_scope, "affine")
        self.generate(expr.then_branch, if_scope)
        
        # CRITICAL: Create convergence constraint
        convergence = f"convergence_{if_scope}"
        self.add_constraint(convergence, if_scope)
        
        # Both branches connect to convergence
        self.add_incidence(then_result, convergence)
        self.add_incidence(else_result, convergence)
        
        # This creates cycle: test → branches → convergence → back
        test_var = f"test_{if_scope}"
        self.add_binding(test_var, if_scope, "affine")
        self.add_incidence(test_var, convergence)
```

## Expected Results After Fix

### Example 1: Factorial (Direct Recursion)

**Before:** H¹ = 0
**After:** H¹ = 1

**Why:**
```
Cycle detected:
  factorial → recursion_point → cycle_factorial → recursion_point → factorial
```

### Example 2: Y-Combinator

**Before:** H¹ = 0  
**After:** H¹ = 1

**Why:**
```
Cycle detected:
  fixed_point → y_fixed_point → fixed_point
  (self-loop in incidence structure)
```

### Example 3: Safe Divide

**Before:** H¹ = 0
**After:** H¹ = 1

**Why:**
```
Cycle detected:
  test → else(∞) → convergence → then → convergence → test
  (cycle through projective point)
```

## Implementation Priority

1. **High Priority** (solves research question):
   - Recursive call detection
   - Self-recursion cycles
   
2. **Medium Priority** (validates theory):
   - Projective closure cycles
   - Branch convergence
   
3. **Low Priority** (nice to have):
   - Y-combinator self-loops
   - Full combinator library

## The Geometric Intuition

Your trigonometry analogy is perfect:

```
CARTESIAN (direct)         POLAR (via combinators)
     ↓                           ↓
  factorial(n)              Y (λf λn. ...)
     ↓                           ↓
  n * fac(n-1)              f = Y f
     ↓                           ↓
EXPLICIT CYCLE            IMPLICIT CYCLE
  (easy to see)           (needs conversion)
```

**Both should give H¹ > 0**, but we need to properly "convert coordinates" by:
- Detecting the recursive call → explicit cycle
- Recognizing Y-combinator → implicit cycle
- Converting both to incidence structure

## Next Steps

1. **Implement Fix 1-4** in `scheme_h1_pipeline.py`
2. **Re-run examples** - should see H¹ > 0
3. **Test on real Scheme corpus** - validate hypothesis
4. **Compare affine vs projective** - measure improvement

## The Trigonometric Transform

This is exactly like:

```
sin²θ + cos²θ = 1    (Pythagorean identity)
      ↓
Converts between coordinate systems
      ↓
Direct recursion + Y-combinator = Same H¹
      (both should detect the cycle)
```

The Y-combinator is the "trigonometric function" that reveals the hidden cyclic structure!

---

**TL;DR:** We built the pipeline correctly, but we're not creating enough incidence edges to detect cycles. Need to add:
1. Recursive call dependencies
2. Y-combinator self-loops  
3. Projective closure cycles

Then H¹ will increase from 0 to positive values, answering your research question!
