#lang racket/base

(require "src/nlp/nlp-main.rkt")

;; Test NLP augmentation capabilities

(printf "=== Testing NLP Augmentation ===\n\n")

;; Test 1: Synonym expansion
(printf "Test 1: Synonym Expansion\n")
(define tokens1 (tokenize "calculate H1 for program test"))
(printf "Input: 'calculate H1 for program test'\n")
(printf "Tokens: ~a\n\n" tokens1)

;; Test 2: Fuzzy matching
(printf "Test 2: Fuzzy Matching (Typo Tolerance)\n")
(define tokens2 (tokenize "comptue vg for program test"))
(printf "Input: 'comptue vg for program test' (typo: comptue)\n")
(printf "Tokens: ~a\n\n" tokens2)

;; Test 3: Context expansion
(printf "Test 3: Context-Aware Expansion\n")
(define tokens3 (tokenize "compute cohomology for program test"))
(printf "Input: 'compute cohomology for program test'\n")
(printf "Tokens: ~a\n\n" tokens3)

;; Test 4: Phrase handling
(printf "Test 4: Multi-word Phrase Handling\n")
(define tokens4 (tokenize "export polynomial representation for program test"))
(printf "Input: 'export polynomial representation for program test'\n")
(printf "Tokens: ~a\n\n" tokens4)

;; Test 5: Semantic lattice
(printf "Test 5: Semantic Lattice Initialization\n")
(define lattice (initialize-domain-knowledge))
(printf "Lattice nodes: ~a\n" (set-count (semantic-lattice-nodes lattice)))
(printf "Lattice initialized successfully!\n\n")

;; Test 6: Full pipeline
(printf "Test 6: Full Pipeline with Augmentation\n")
(define test-queries
  '("calculate H1 for program test"
    "fetch pattern dimensions for program test"
    "get polynomial for program test"
    "verify hypothesis H1 = V(G) for program test"))

(for ([query test-queries])
  (printf "Query: ~a\n" query)
  (define-values (frame _) (parse-query query))
  (printf "  Parsed successfully: ~a\n" (if frame "Yes" "No"))
  (if frame
      (printf "  Concepts: ~a\n" (semantic-frame-concepts frame))
      (printf "  Failed to parse\n"))
  (printf "\n"))

(printf "=== All Tests Complete ===\n")


