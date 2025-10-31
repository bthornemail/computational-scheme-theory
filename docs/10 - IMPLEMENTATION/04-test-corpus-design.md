# Test Corpus Design - 350-Program Validation Suite

**Status:** Planning Phase
**Purpose:** Systematically validate the H¹ = V(G) - k hypothesis across diverse R5RS Scheme programs

---

## 1. Corpus Overview

The test corpus consists of **350 carefully designed R5RS Scheme programs** organized into 7 categories, each targeting specific computational patterns to thoroughly test the hypothesis.

### Success Criteria

- **Correlation > 0.9**: Strong statistical relationship between H¹ and V(G)
- **80%+ exact matches**: After k-normalization, H¹ = V(G) - k holds
- **Clear failure patterns**: Any discrepancies follow predictable rules

---

## 2. Category Breakdown

| Category | Count | Expected H¹ Range | Purpose |
|----------|-------|-------------------|---------|
| Baseline | 20 | 0-1 | Establish minimum complexity (no control flow) |
| Simple Control | 50 | 2-5 | Single branching/looping constructs |
| Recursion | 50 | 2-10 | Test back-edges from recursive calls |
| Complex Control | 50 | 5-15 | Nested structures, multiple paths |
| Functional | 50 | 3-12 | Higher-order functions, closures |
| call/cc | 30 | ??? | First-class continuations (exploratory) |
| Real Programs | 100 | 1-50 | Open-source Scheme code from GitHub |
| **Total** | **350** | | |

---

## 3. Detailed Category Specifications

### 3.1 Baseline (20 programs)

**Purpose**: Establish that straight-line code has H¹ = 0 or 1 (no cycles)

**Characteristics**:
- No control flow (no if, cond, loops)
- Only sequential computation
- Expected: V(G) = 1, H¹ = 1 (or 0 depending on normalization)

**Examples**:

```scheme
;; B001: Simple arithmetic
(define (add-three x) (+ x 3))

;; B002: Multiple statements
(define (compute x y)
  (let ((a (+ x y))
        (b (* x y)))
    (/ a b)))

;; B003: Nested let bindings
(define (nested x)
  (let ((a x))
    (let ((b (+ a 1)))
      (let ((c (+ b 1)))
        c))))

;; B004: Lambda without application
(define identity (lambda (x) x))

;; B005: Multiple defines
(define pi 3.14159)
(define e 2.71828)
(define golden-ratio 1.61803)
```

**Test Matrix**:
1. Single expression (5 programs)
2. Multiple expressions (5 programs)
3. Nested bindings (5 programs)
4. Lambda definitions (5 programs)

---

### 3.2 Simple Control (50 programs)

**Purpose**: Test single branching or looping constructs

**Characteristics**:
- One if/cond/case statement
- One loop (iterative or recursive)
- Expected: V(G) = 2, H¹ = 2 (or 1 after normalization)

**Examples**:

```scheme
;; SC001: Simple if (binary branch)
(define (abs x)
  (if (< x 0)
      (- x)
      x))

;; SC002: If with complex branches
(define (sign x)
  (if (< x 0)
      (- 0 1)
      (if (> x 0)
          1
          0)))

;; SC003: Cond with 3 clauses
(define (classify x)
  (cond
    ((< x 0) 'negative)
    ((= x 0) 'zero)
    ((> x 0) 'positive)))

;; SC004: Simple recursion
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; SC005: Tail recursion
(define (sum n acc)
  (if (= n 0)
      acc
      (sum (- n 1) (+ acc n))))
```

**Test Matrix**:
- if statements (2-way branches): 10 programs
- cond statements (3+ way branches): 10 programs
- case statements: 5 programs
- Simple recursion: 15 programs
- Tail recursion: 10 programs

---

### 3.3 Recursion (50 programs)

**Purpose**: Test back-edges created by recursive function calls

**Characteristics**:
- Direct recursion, mutual recursion, nested recursion
- Expected: H¹ should count recursive cycles

**Examples**:

```scheme
;; R001: Fibonacci (2 recursive calls)
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; R002: Ackermann function (nested recursion)
(define (ackermann m n)
  (cond
    ((= m 0) (+ n 1))
    ((= n 0) (ackermann (- m 1) 1))
    (else (ackermann (- m 1)
                     (ackermann m (- n 1))))))

;; R003: Mutual recursion (even/odd)
(define (even? n)
  (if (= n 0)
      #t
      (odd? (- n 1))))

(define (odd? n)
  (if (= n 0)
      #f
      (even? (- n 1))))

;; R004: Tree traversal
(define (tree-sum tree)
  (if (null? tree)
      0
      (if (pair? tree)
          (+ (tree-sum (car tree))
             (tree-sum (cdr tree)))
          tree)))

;; R005: List recursion
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))
```

**Test Matrix**:
- Single recursive call: 10 programs
- Multiple recursive calls (like fib): 10 programs
- Mutual recursion: 10 programs
- Nested recursion: 10 programs
- Structural recursion (trees/lists): 10 programs

---

### 3.4 Complex Control (50 programs)

**Purpose**: Test nested and compound control structures

**Characteristics**:
- Nested if/cond/loops
- Multiple independent branches
- Complex predicates
- Expected: H¹ = V(G) should hold for well-structured complexity

**Examples**:

```scheme
;; CC001: Nested if statements
(define (nested-if a b c)
  (if (> a 0)
      (if (> b 0)
          (if (> c 0)
              'all-positive
              'c-negative)
          'b-negative)
      'a-negative))

;; CC002: Multiple sequential branches
(define (process-input x)
  (if (negative? x)
      (set! x (abs x))
      #f)
  (if (even? x)
      (set! x (/ x 2))
      (set! x (* x 3)))
  x)

;; CC003: Nested loops (via recursion)
(define (nested-sum n m)
  (define (outer i)
    (if (= i n)
        0
        (+ (inner i 0) (outer (+ i 1)))))
  (define (inner i j)
    (if (= j m)
        0
        (+ (* i j) (inner i (+ j 1)))))
  (outer 0))

;; CC004: Complex cond with nested logic
(define (grade score)
  (cond
    ((>= score 90)
     (if (>= score 95) 'A+ 'A))
    ((>= score 80)
     (if (>= score 85) 'B+ 'B))
    ((>= score 70)
     'C)
    (else
     (if (>= score 60) 'D 'F))))

;; CC005: State machine
(define (parse-number str state)
  (cond
    ((null? str) (if (eq? state 'valid) 'done 'error))
    ((eq? state 'start)
     (if (digit? (car str))
         (parse-number (cdr str) 'valid)
         (parse-number (cdr str) 'error)))
    ((eq? state 'valid)
     (if (digit? (car str))
         (parse-number (cdr str) 'valid)
         (parse-number (cdr str) 'error)))
    ((eq? state 'error) 'error)))
```

**Test Matrix**:
- Nested if (depth 2-4): 10 programs
- Sequential branches: 10 programs
- Nested loops: 10 programs
- Complex cond: 10 programs
- State machines: 10 programs

---

### 3.5 Functional (50 programs)

**Purpose**: Test higher-order functions and closures

**Characteristics**:
- map, fold, filter implementations/usage
- Currying and partial application
- Closures with captured state
- Expected: Binding complexity increases, test if H¹ tracks this

**Examples**:

```scheme
;; F001: Map implementation
(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map f (cdr lst)))))

;; F002: Fold/reduce implementation
(define (fold-left f acc lst)
  (if (null? lst)
      acc
      (fold-left f
                 (f acc (car lst))
                 (cdr lst))))

;; F003: Filter implementation
(define (filter pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst))
     (cons (car lst) (filter pred (cdr lst))))
    (else (filter pred (cdr lst)))))

;; F004: Closure example
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

;; F005: Currying
(define (curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

;; F006: Compose
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; F007: Y-combinator (fixed-point)
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))
```

**Test Matrix**:
- map/filter/fold implementations: 10 programs
- Higher-order function usage: 10 programs
- Closures: 10 programs
- Currying/partial application: 10 programs
- Combinators (Y, S, K, etc.): 10 programs

---

### 3.6 call/cc (30 programs)

**Purpose**: Test first-class continuations (the hard case)

**Characteristics**:
- Non-local control flow via call/cc
- Backtracking, exceptions, coroutines
- Expected: **This is exploratory** - unclear if H¹ = V(G) holds

**Examples**:

```scheme
;; CCC001: Simple early return
(define (find-positive lst)
  (call/cc
    (lambda (return)
      (for-each
        (lambda (x)
          (if (positive? x)
              (return x)))
        lst)
      #f)))

;; CCC002: Exception handling (via continuation)
(define (safe-divide a b)
  (call/cc
    (lambda (return)
      (if (= b 0)
          (return 'error)
          (/ a b)))))

;; CCC003: Backtracking search
(define (amb choices)
  (call/cc
    (lambda (cc)
      (for-each cc choices)
      (error "No choices"))))

;; CCC004: Coroutine simulation
(define (make-coroutine proc)
  (let ((resume #f)
        (yield #f))
    (call/cc
      (lambda (caller)
        (set! resume caller)
        (proc)))))

;; CCC005: Tree traversal with early exit
(define (tree-find pred tree)
  (call/cc
    (lambda (return)
      (define (search t)
        (cond
          ((null? t) #f)
          ((pred t) (return t))
          ((pair? t)
           (search (car t))
           (search (cdr t)))))
      (search tree)
      #f)))
```

**Test Matrix**:
- Early return: 5 programs
- Exception handling: 5 programs
- Backtracking: 5 programs
- Coroutines: 5 programs
- Generators: 5 programs
- Complex call/cc: 5 programs

**Research Questions**:
1. Does call/cc create additional "holes" in the topological space?
2. Is V(G) even well-defined for non-local control flow?
3. Do we need to extend the binding algebra to model continuations?

---

### 3.7 Real Programs (100 programs)

**Purpose**: Test on authentic, production-quality Scheme code

**Sources**:
1. **SICP exercises** (20 programs) - well-tested, documented
2. **Rosetta Code** (20 programs) - standard algorithm implementations
3. **GitHub repos** (40 programs) - open-source Scheme projects
4. **Scheme benchmarks** (20 programs) - performance test suite

**Selection Criteria**:
- Must be valid R5RS Scheme
- Size: 10-500 LOC
- Diverse problem domains
- Well-commented (for debugging failures)

**Example Sources**:

```yaml
# sources.yaml
sicp:
  - url: https://github.com/sarabander/sicp/
    programs:
      - ch2/2.1/rational.scm
      - ch2/2.2/list-ops.scm
      - ch2/2.3/symbolic-diff.scm
      - ch3/3.1/local-state.scm
      - ch3/3.5/streams.scm

rosetta_code:
  - url: https://rosettacode.org/wiki/Category:Scheme
    programs:
      - quicksort
      - merge-sort
      - binary-search
      - huffman-coding
      - dijkstra-algorithm

github:
  repos:
    - owner: ashinn
      repo: chibi-scheme
      files:
        - lib/init-7.scm
        - lib/srfi/1.scm  # List library
    - owner: cisco
      repo: ChezScheme
      files:
        - s/5_1.ss  # Compiler internals
    - owner: scheme-requests-for-implementation
      repo: srfi-common
      files:
        - srfi-1-reference.scm  # Lists
        - srfi-13-reference.scm # Strings

benchmarks:
  - url: https://github.com/ecraven/r7rs-benchmarks
    programs:
      - src/ack.scm
      - src/fib.scm
      - src/tak.scm
      - src/array1.scm
```

**Test Matrix**:
- 10-50 LOC: 30 programs
- 51-100 LOC: 40 programs
- 101-200 LOC: 20 programs
- 201-500 LOC: 10 programs

---

## 4. Corpus File Structure

```
test-corpus/
├── metadata.json                    # Overall corpus metadata
├── baseline/
│   ├── B001-simple-arithmetic.scm
│   ├── B002-multiple-statements.scm
│   ├── ...
│   └── metadata/
│       ├── B001.json                # Expected results
│       ├── B002.json
│       └── ...
├── simple-control/
│   ├── SC001-simple-if.scm
│   ├── SC002-nested-if.scm
│   ├── ...
│   └── metadata/
├── recursion/
│   ├── R001-factorial.scm
│   ├── R002-fibonacci.scm
│   ├── ...
│   └── metadata/
├── complex-control/
│   ├── CC001-nested-if.scm
│   ├── ...
│   └── metadata/
├── functional/
│   ├── F001-map.scm
│   ├── ...
│   └── metadata/
├── call-cc/
│   ├── CCC001-early-return.scm
│   ├── ...
│   └── metadata/
├── real-programs/
│   ├── RP001-sicp-rational.scm
│   ├── RP002-rosetta-quicksort.scm
│   ├── ...
│   └── metadata/
└── scripts/
    ├── generate_corpus.py           # Generate synthetic programs
    ├── fetch_real_programs.py       # Download from GitHub
    ├── validate_corpus.py           # Check all programs parse
    └── run_validation.py            # Run full validation suite
```

---

## 5. Program Metadata Format

Each program has an associated JSON metadata file:

```json
{
  "program_id": "SC001",
  "category": "simple-control",
  "name": "simple-if",
  "description": "Binary branch with simple if statement",
  "source_file": "simple-control/SC001-simple-if.scm",

  "expected_results": {
    "v_g": 2,
    "h1_min": 1,
    "h1_max": 2,
    "hypothesis_should_hold": true,
    "expected_k": 1
  },

  "features": {
    "has_recursion": false,
    "has_call_cc": false,
    "has_set": false,
    "has_macros": false,
    "control_constructs": ["if"],
    "num_bindings": 1,
    "loc": 4
  },

  "tags": ["basic", "if-statement", "binary-branch"],

  "notes": "Simple test case for basic branching. Should have V(G)=2 and H¹=1 or 2.",

  "author": "Generated",
  "date_added": "2025-10-31"
}
```

---

## 6. Corpus Generation Strategy

### 6.1 Synthetic Programs (250 programs)

**Tool**: `scripts/generate_corpus.py`

```python
# generate_corpus.py
import random
from typing import List

class SchemeGenerator:
    """Generate synthetic R5RS Scheme programs"""

    def generate_baseline(self, num: int) -> List[str]:
        """Generate straight-line programs"""
        programs = []
        for i in range(num):
            if i % 4 == 0:
                programs.append(self._gen_arithmetic())
            elif i % 4 == 1:
                programs.append(self._gen_let_bindings())
            elif i % 4 == 2:
                programs.append(self._gen_lambda_def())
            else:
                programs.append(self._gen_sequence())
        return programs

    def generate_simple_control(self, num: int) -> List[str]:
        """Generate programs with single control construct"""
        programs = []
        for i in range(num):
            if i % 5 == 0:
                programs.append(self._gen_if())
            elif i % 5 == 1:
                programs.append(self._gen_cond())
            elif i % 5 == 2:
                programs.append(self._gen_simple_recursion())
            # ... etc
        return programs

    def _gen_if(self) -> str:
        """Generate a simple if statement"""
        var = random.choice(['x', 'n', 'value'])
        op = random.choice(['<', '>', '=', '<=', '>='])
        threshold = random.randint(-10, 10)
        return f"""(define (test-{var} {var})
  (if ({op} {var} {threshold})
      'yes
      'no))"""
```

**Validation**: Each generated program must:
1. Parse as valid R5RS
2. Have known V(G) (computed manually for templates)
3. Have unique structure (no duplicates)

### 6.2 Real Programs (100 programs)

**Tool**: `scripts/fetch_real_programs.py`

```python
# fetch_real_programs.py
import requests
import yaml

def fetch_github_program(owner, repo, path):
    """Download a Scheme file from GitHub"""
    url = f"https://raw.githubusercontent.com/{owner}/{repo}/master/{path}"
    response = requests.get(url)
    if response.status_code == 200:
        return response.text
    else:
        raise Exception(f"Failed to fetch {url}")

def load_sources():
    """Load sources.yaml configuration"""
    with open('sources.yaml') as f:
        return yaml.safe_load(f)

def main():
    sources = load_sources()
    for repo in sources['github']['repos']:
        owner = repo['owner']
        repo_name = repo['repo']
        for file_path in repo['files']:
            try:
                code = fetch_github_program(owner, repo_name, file_path)
                # Save to corpus
                save_program(code, owner, repo_name, file_path)
            except Exception as e:
                print(f"Error: {e}")
```

---

## 7. Validation Workflow

### 7.1 Single Program Validation

```bash
# Validate one program
./scripts/validate_program.sh test-corpus/simple-control/SC001-simple-if.scm

# Output:
# Program ID: SC001
# Category: simple-control
# H¹: 2
# V(G): 2
# Hypothesis holds: YES (k=0)
# Time: 42ms
```

### 7.2 Full Corpus Validation

```bash
# Run entire corpus
./scripts/run_validation.py --corpus test-corpus/ --parallel --workers 8

# Output:
# [====================] 350/350 (100%)
# Total time: 5m 32s
# Succeeded: 342
# Failed: 8
# Hypothesis holds: 327/342 (95.6%)
# Mean |H¹ - V(G)|: 0.23
# Std dev: 0.67
```

### 7.3 Analysis and Reporting

```python
# scripts/analyze_results.py
import pandas as pd
import matplotlib.pyplot as plt

def analyze_corpus_results(results_csv):
    df = pd.read_csv(results_csv)

    # Correlation
    correlation = df['h1'].corr(df['v_g'])
    print(f"Correlation(H¹, V(G)): {correlation:.4f}")

    # Scatter plot
    plt.scatter(df['v_g'], df['h1'])
    plt.xlabel('V(G) (Cyclomatic Complexity)')
    plt.ylabel('H¹ (Cohomology)')
    plt.title('H¹ vs V(G) for 350-Program Corpus')
    plt.plot([0, df['v_g'].max()], [0, df['v_g'].max()], 'r--', label='y=x')
    plt.legend()
    plt.savefig('h1_vs_vg.png')

    # By category
    category_stats = df.groupby('category').agg({
        'h1': ['mean', 'std'],
        'v_g': ['mean', 'std'],
        'hypothesis_holds': 'mean'
    })
    print("\nBy Category:")
    print(category_stats)

    # Failure analysis
    failures = df[~df['hypothesis_holds']]
    print(f"\n{len(failures)} failures:")
    print(failures[['program_id', 'h1', 'v_g', 'difference']])
```

---

## 8. Quality Assurance

### 8.1 Corpus Validation Checks

**Pre-validation** (before running experiments):

1. **Syntax check**: All programs parse as R5RS
2. **No duplicates**: Each program has unique structure
3. **Size distribution**: Programs cover expected LOC ranges
4. **Feature coverage**: All R5RS features represented
5. **Metadata complete**: All JSON files present and valid

**Script**: `scripts/validate_corpus.py`

### 8.2 Ground Truth Verification

For each category, manually compute H¹ and V(G) for **at least 3 programs** to verify:
1. Implementation is correct
2. Expected values in metadata are accurate
3. Any discrepancies are understood

---

## 9. Open Questions & Future Extensions

### Research Questions

1. **Optimal k value**: Is k constant across all programs, or does it depend on features?
2. **call/cc handling**: Do continuations break the H¹ = V(G) correspondence?
3. **Macros**: Should macros be expanded before or after α-conversion?
4. **Higher cohomology**: What do H², H³ represent computationally?

### Future Corpus Extensions

1. **Other Scheme dialects**: Test on Racket, Guile, Chicken
2. **Other languages**: OCaml, Haskell, SML
3. **Larger programs**: 1000+ LOC real-world applications
4. **Mutating state**: Programs with set! (currently underrepresented)

---

## 10. Next Steps

1. **Create directory structure** (`test-corpus/` hierarchy)
2. **Implement corpus generator** (`generate_corpus.py`)
3. **Fetch real programs** (`fetch_real_programs.py`)
4. **Validate corpus** (syntax checks, metadata)
5. **Run pilot study** (first 50 programs)
6. **Analyze results** (correlation, failures)
7. **Iterate and expand**

---

## 11. Timeline Estimate

- **Week 1-2**: Directory structure + metadata schemas
- **Week 3-4**: Generate synthetic programs (250)
- **Week 5-6**: Fetch and curate real programs (100)
- **Week 7**: Validation and quality assurance
- **Week 8**: Pilot run on first 50 programs
- **Week 9-10**: Full corpus validation (350 programs)
- **Week 11-12**: Analysis and reporting

**Total**: ~3 months for complete test corpus development and validation
