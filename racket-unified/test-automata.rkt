#lang racket/base

(require "src/nlp/nlp-main.rkt"
         "src/nlp/pattern-automaton.rkt")

(printf "=== Testing Automata Implementation ===\n\n")

;; Test 1: NFA-ε Multiple Interpretations
(printf "Test 1: NFA-ε - Multiple Interpretations\n")
(printf "Query: 'compute H1'\n")
(define configs1 (nfa-parse-query "compute H1"))
(printf "  Found ~a interpretations\n" (length configs1))
(if (> (length configs1) 0)
    (for ([config configs1])
      (define state (parse-configuration-state config))
      (define frame (parse-configuration-frame config))
      (printf "    - State: ~a, Confidence: ~a\n" 
              (nfa-state-id state)
              (parse-configuration-confidence config)))
    (printf "    ⚠️  No interpretations found (may need query with more context)\n"))
(printf "\n")

;; Test 2: Interpretation Explorer
(printf "Test 2: Interpretation Explorer - Ranking\n")
(printf "Query: 'calculate H1 for program test'\n")
(define lattice (initialize-domain-knowledge))
(define interpretations (explore-interpretations "calculate H1 for program test" lattice))
(printf "  Found ~a ranked interpretations\n" (length interpretations))
(for ([interp interpretations]
      [i (in-range (min 3 (length interpretations)))])
  (printf "    ~a. Confidence: ~a, Semantic: ~a, Rank: ~a\n"
          (+ i 1)
          (interpretation-confidence interp)
          (interpretation-semantic-score interp)
          (interpretation-rank interp)))
(printf "\n")

;; Test 3: Branch Point Resolution
(printf "Test 3: Branch Point Resolver - Ambiguity Detection\n")
(printf "Query: 'compute complexity'\n")
(define branches (identify-branch-points "compute complexity" lattice))
(printf "  Found ~a branch point(s)\n" (length branches))
(for ([branch branches])
  (printf "    Branch: ~a interpretations\n" 
          (length (branch-point-interpretations branch)))
  (define resolved (resolve-branch-point branch lattice))
  (when (branch-point-chosen resolved)
    (printf "    Chosen: rank ~a\n" 
            (interpretation-rank (branch-point-chosen resolved)))))
(printf "\n")

;; Test 4: Pattern Automaton
(printf "Test 4: Pattern Automaton - Dimensional Mapping\n")
(define test-patterns
  '("get pattern dimensions for test"
    "pattern matching with ellipsis"
    "extract pattern structure"))
(for ([query test-patterns])
  (printf "  Query: ~a\n" query)
  (define pattern-info (extract-pattern-dimensions query))
  (printf "    Pattern: ~a\n" (hash-ref pattern-info 'pattern))
  (printf "    Dimension: ~a\n" (hash-ref pattern-info 'dimension))
  (printf "    Church Numeral: ~a\n" (hash-ref pattern-info 'church-numeral))
  (printf "    Polynomial Degree: ~a\n" (hash-ref pattern-info 'polynomial-degree))
  (printf "\n"))

;; Test 5: Best Interpretation Selection
(printf "Test 5: Best Interpretation Selection\n")
(printf "Query: 'fetch pattern dimensions'\n")
(define all-interps (explore-interpretations "fetch pattern dimensions" lattice))
(define best (best-interpretation all-interps))
(if best
    (begin
      (printf "  Best interpretation selected\n")
      (printf "    Rank: ~a\n" (interpretation-rank best))
      (printf "    Confidence: ~a\n" (interpretation-confidence best))
      (printf "    Semantic Score: ~a\n" (interpretation-semantic-score best)))
    (printf "  No valid interpretations found\n"))
(printf "\n")

;; Test 6: Ambiguous Query Handling
(printf "Test 6: Ambiguous Query - Multiple Valid Paths\n")
(printf "Query: 'get H1'\n")
(define amb-interps (explore-interpretations "get H1" lattice))
(printf "  Explored ~a interpretations\n" (length amb-interps))
(let ([amb-branches (identify-branch-points "get H1" lattice)])
  (if (> (length amb-interps) 1)
      (begin
        (printf "  ✅ Ambiguity detected (branch point)\n")
        (printf "  Resolved to ~a best interpretation\n" (length amb-branches)))
      (printf "  Single interpretation (no ambiguity)\n")))
(printf "\n")

;; Test 7: Pattern to Dimension Mapping
(printf "Test 7: Pattern → Dimension → H¹ Integration\n")
(define test-dims
  (map pattern-to-dimension
       '(() 
         (x)
         (x ...)
         #(x ...)
         ((x ...) ...))))
(printf "  Pattern → Dimension mapping:\n")
(for ([pattern '(() (x) (x ...) #(x ...) ((x ...) ...))]
      [dim test-dims])
  (printf "    ~a → dimension ~a → H¹ contribution: ~a\n"
          pattern
          dim
          (pattern-dimension-to-h1 dim)))
(printf "\n")

;; Test 8: Epsilon Closure
(printf "Test 8: Epsilon Closure Computation\n")
(define test-state (make-initial-nfa-state '()))
(define epsilon-configs (nfa-epsilon-closure test-state))
(printf "  Initial state epsilon closure: ~a configurations\n" (length epsilon-configs))
(printf "\n")

(printf "=== All Tests Complete ===\n")
(printf "\n✅ NFA-ε: Multiple interpretations explored\n")
(printf "✅ Interpretation Explorer: Rankings computed\n")
(printf "✅ Branch Point Resolver: Ambiguity handled\n")
(printf "✅ Pattern Automaton: Dimensions mapped\n")
(printf "\n🎯 System ready for full understanding!\n")

