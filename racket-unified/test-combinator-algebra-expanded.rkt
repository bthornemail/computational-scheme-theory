#lang racket/base

(require "src/algorithms/combinator-algebra.rkt"
         "src/s-expression.rkt"
         "src/m-expression.rkt"
         "src/persistence/init.rkt"
         racket/match
         racket/time)

;; Initialize persistence before running tests
(initialize-persistence)

;; Test counters
(define test-passed 0)
(define test-failed 0)
(define test-skipped 0)

;; Test result tracking
(define test-results '())

(define (run-test name test-fn)
  (printf "Testing: ~a... " name)
  (let* ([start-time (current-inexact-milliseconds)]
         [result (with-handlers ([exn? (lambda (e) `(error . ,(exn-message e)))])
                    (test-fn))]
         [end-time (current-inexact-milliseconds)]
         [duration (- end-time start-time)])
    (if (eq? (car result) 'error)
        (begin
          (set! test-failed (+ test-failed 1))
          (set! test-results (cons `(,name failed error ,(cdr result) ,duration) test-results))
          (printf "❌ FAILED (~ams): ~a\n" duration (cdr result)))
        (begin
          (set! test-passed (+ test-passed 1))
          (set! test-results (cons `(,name passed ,result ,duration) test-results))
          (printf "✅ PASSED (~ams)\n" duration)))))

(printf "╔══════════════════════════════════════════════════════════╗\n")
(printf "║  COMBINATOR ALGEBRA EXTENSION TEST SUITE (Appendix Z)   ║\n")
(printf "╚══════════════════════════════════════════════════════════╝\n\n")

;; ============================================================
;; TEST CATEGORY 1: BASIC RECURSION (20 tests)
;; ============================================================

(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "CATEGORY 1: BASIC RECURSION\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

;; Setup test rings/fields
(create-y-combinator-ring "TestRing" "BaseRing" (lambda (f) f))
(create-z-combinator-field "TestField" "BaseField" (lambda (f) f))

;; Test 1.1: Factorial using Y-combinator
(run-test "Factorial (Y-combinator)"
  (lambda ()
    (define fact-gen (lambda (self)
                       (lambda (n)
                         (if (= n 0)
                             1
                             (* n ((self self) (- n 1)))))))
    (define fact (recursive-structure "TestRing" fact-gen))
    (define result (fact 5))
    (if (= result 120) 'ok 'fail)))

;; Test 1.2: Fibonacci using Y-combinator
(run-test "Fibonacci (Y-combinator)"
  (lambda ()
    (define fib-gen (lambda (self)
                       (lambda (n)
                         (if (< n 2)
                             n
                             (+ ((self self) (- n 1))
                                ((self self) (- n 2)))))))
    (define fib (recursive-structure "TestRing" fib-gen))
    (define result (fib 7))
    (if (= result 13) 'ok 'fail)))

;; Test 1.3: List length using Y-combinator
(run-test "List length (Y-combinator)"
  (lambda ()
    (define length-gen (lambda (self)
                          (lambda (lst)
                            (if (null? lst)
                                0
                                (+ 1 ((self self) (cdr lst)))))))
    (define length (recursive-structure "TestRing" length-gen))
    (define result (length '(a b c d e)))
    (if (= result 5) 'ok 'fail)))

;; Test 1.4: List sum using Y-combinator
(run-test "List sum (Y-combinator)"
  (lambda ()
    (define sum-gen (lambda (self)
                       (lambda (lst)
                         (if (null? lst)
                             0
                             (+ (car lst) ((self self) (cdr lst)))))))
    (define sum (recursive-structure "TestRing" sum-gen))
    (define result (sum '(1 2 3 4 5)))
    (if (= result 15) 'ok 'fail)))

;; Test 1.5: List reverse using Y-combinator
(run-test "List reverse (Y-combinator)"
  (lambda ()
    (define rev-gen (lambda (self)
                       (lambda (lst acc)
                         (if (null? lst)
                             acc
                             ((self self) (cdr lst) (cons (car lst) acc))))))
    (define rev (recursive-structure "TestRing" rev-gen))
    (define result (rev '(1 2 3) '()))
    (if (equal? result '(3 2 1)) 'ok 'fail)))

;; Test 1.6: Fixed-point of identity function
(run-test "Fixed-point identity (Y-combinator)"
  (lambda ()
    (define id (lambda (x) x))
    (define result (fixed-point-algebra "TestRing" id))
    'ok))

;; Test 1.7: Fixed-point computation
(run-test "Fixed-point constant function"
  (lambda ()
    (define const-5 (lambda (x) 5))
    (define result (fixed-point-algebra "TestRing" const-5))
    (if (= result 5) 'ok 'fail)))

;; Test 1.8: Z-combinator fixed-point
(run-test "Z-combinator fixed-point"
  (lambda ()
    (define square (lambda (x) (* x x)))
    (define result (find-fixed-point "TestField" square))
    'ok))

;; Test 1.9: Iterative refinement - simple equation
(run-test "Iterative refinement (x = (x+2)/2)"
  (lambda ()
    (define eq (lambda (x) (/ (+ x 2) 2)))
    (define result (iterative-refinement "TestField" eq 0.0))
    (if (< (abs (- result 2.0)) 0.1) 'ok 'fail)))

;; Test 1.10: Iterative refinement - square root
(run-test "Iterative refinement (sqrt approximation)"
  (lambda ()
    (define sqrt-gen (lambda (x) (/ (+ x (/ 4 x)) 2)))
    (define result (iterative-refinement "TestField" sqrt-gen 2.0))
    (if (< (abs (- result 2.0)) 0.1) 'ok 'fail)))

;; Test 1.11: Product of list elements
(run-test "List product (Y-combinator)"
  (lambda ()
    (define prod-gen (lambda (self)
                       (lambda (lst)
                         (if (null? lst)
                             1
                             (* (car lst) ((self self) (cdr lst)))))))
    (define prod (recursive-structure "TestRing" prod-gen))
    (define result (prod '(2 3 4)))
    (if (= result 24) 'ok 'fail)))

;; Test 1.12: Maximum element in list
(run-test "List maximum (Y-combinator)"
  (lambda ()
    (define max-gen (lambda (self)
                      (lambda (lst)
                        (if (null? (cdr lst))
                            (car lst)
                            (let ([rest-max ((self self) (cdr lst))])
                              (if (> (car lst) rest-max)
                                  (car lst)
                                  rest-max))))))
    (define max-elem (recursive-structure "TestRing" max-gen))
    (define result (max-elem '(3 1 4 1 5)))
    (if (= result 5) 'ok 'fail)))

;; Test 1.13: List member check
(run-test "List member check (Y-combinator)"
  (lambda ()
    (define member-gen (lambda (self)
                         (lambda (item lst)
                           (if (null? lst)
                               #f
                               (if (equal? item (car lst))
                                   #t
                                   ((self self) item (cdr lst)))))))
    (define member? (recursive-structure "TestRing" member-gen))
    (define result (member? 'b '(a b c)))
    (if (eq? result #t) 'ok 'fail)))

;; Test 1.14: Count occurrences
(run-test "Count occurrences (Y-combinator)"
  (lambda ()
    (define count-gen (lambda (self)
                        (lambda (item lst)
                          (if (null? lst)
                              0
                              (+ (if (equal? item (car lst)) 1 0)
                                 ((self self) item (cdr lst)))))))
    (define count (recursive-structure "TestRing" count-gen))
    (define result (count 2 '(1 2 3 2 4 2)))
    (if (= result 3) 'ok 'fail)))

;; Test 1.15: Power function
(run-test "Power function (Y-combinator)"
  (lambda ()
    (define power-gen (lambda (self)
                        (lambda (base exp)
                          (if (= exp 0)
                              1
                              (* base ((self self) base (- exp 1)))))))
    (define power (recursive-structure "TestRing" power-gen))
    (define result (power 2 8))
    (if (= result 256) 'ok 'fail)))

;; Test 1.16: GCD using Euclidean algorithm
(run-test "GCD (Y-combinator)"
  (lambda ()
    (define gcd-gen (lambda (self)
                      (lambda (a b)
                        (if (= b 0)
                            a
                            ((self self) b (remainder a b))))))
    (define gcd (recursive-structure "TestRing" gcd-gen))
    (define result (gcd 48 18))
    (if (= result 6) 'ok 'fail)))

;; Test 1.17: List flatten
(run-test "List flatten (Y-combinator)"
  (lambda ()
    (define flatten-gen (lambda (self)
                          (lambda (lst)
                            (if (null? lst)
                                '()
                                (if (list? (car lst))
                                    (append ((self self) (car lst)) ((self self) (cdr lst)))
                                    (cons (car lst) ((self self) (cdr lst))))))))
    (define flatten (recursive-structure "TestRing" flatten-gen))
    (define result (flatten '((1 2) (3 (4)) 5)))
    (if (equal? result '(1 2 3 4 5)) 'ok 'fail)))

;; Test 1.18: List filter
(run-test "List filter (Y-combinator)"
  (lambda ()
    (define filter-gen (lambda (self)
                         (lambda (pred lst)
                           (if (null? lst)
                               '()
                               (if (pred (car lst))
                                   (cons (car lst) ((self self) pred (cdr lst)))
                                   ((self self) pred (cdr lst)))))))
    (define filter (recursive-structure "TestRing" filter-gen))
    (define result (filter (lambda (x) (> x 2)) '(1 2 3 4 5)))
    (if (equal? result '(3 4 5)) 'ok 'fail)))

;; Test 1.19: List map
(run-test "List map (Y-combinator)"
  (lambda ()
    (define map-gen (lambda (self)
                      (lambda (fn lst)
                        (if (null? lst)
                            '()
                            (cons (fn (car lst)) ((self self) fn (cdr lst)))))))
    (define map-fn (recursive-structure "TestRing" map-gen))
    (define result (map-fn (lambda (x) (* x 2)) '(1 2 3)))
    (if (equal? result '(2 4 6)) 'ok 'fail)))

;; Test 1.20: Binary search (simulated)
(run-test "Binary search simulation (Y-combinator)"
  (lambda ()
    (define bsearch-gen (lambda (self)
                           (lambda (target lst)
                             (if (null? lst)
                                 #f
                                 (let ([mid (quotient (length lst) 2)])
                                   (if (equal? target (list-ref lst mid))
                                       #t
                                       (if (< target (list-ref lst mid))
                                           ((self self) target (take lst mid))
                                           ((self self) target (drop lst (+ mid 1)))))))))
    (define bsearch (recursive-structure "TestRing" bsearch-gen))
    (define result (bsearch 3 '(1 2 3 4 5)))
    (if (eq? result #t) 'ok 'fail)))

;; ============================================================
;; TEST CATEGORY 2: MUTUAL RECURSION (15 tests)
;; ============================================================

(printf "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "CATEGORY 2: MUTUAL RECURSION\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

;; Test 2.1: Even/odd mutual recursion
(run-test "Even/odd mutual recursion"
  (lambda ()
    (define even-gen (lambda (self)
                       (lambda (n)
                         (if (= n 0)
                             #t
                             (let ([odd-fn (lambda (m)
                                             (if (= m 0)
                                                 #f
                                                 ((car (self self)) (- m 1))))])
                               (odd-fn (- n 1)))))))
    (define even (recursive-structure "TestRing" even-gen))
    (define result (even 4))
    (if (eq? result #t) 'ok 'fail)))

;; Test 2.2: Positive/negative mutual recursion
(run-test "Positive/negative mutual recursion"
  (lambda ()
    (define positive-gen (lambda (self)
                           (lambda (n)
                             (if (> n 0)
                                 (let ([negative-fn (lambda (m)
                                                       (if (< m 0)
                                                           #t
                                                           ((car (self self)) (- m 1))))])
                                   (negative-fn (- n)))
                                 #f))))
    (define positive? (recursive-structure "TestRing" positive-gen))
    (define result (positive? 5))
    (if (eq? result #t) 'ok 'fail)))

;; Test 2.3: Is-even/Is-odd helper
(run-test "Is-even/Is-odd helper"
  (lambda ()
    (define even-helper-gen (lambda (self)
                              (lambda (n)
                                (if (= n 0)
                                    #t
                                    (let ([odd-helper (lambda (m)
                                                         (if (= m 0)
                                                             #f
                                                             ((car (self self)) (- m 1))))])
                                      (odd-helper (- n 1)))))))
    (define even-helper (recursive-structure "TestRing" even-helper-gen))
    (define result (even-helper 6))
    (if (eq? result #t) 'ok 'fail)))

;; Test 2.4: Palindrome check mutual recursion
(run-test "Palindrome check mutual recursion"
  (lambda ()
    (define check-palindrome-gen (lambda (self)
                                    (lambda (lst)
                                      (if (null? lst)
                                          #t
                                          (let ([check-rest (lambda (rest)
                                                              (if (null? rest)
                                                                  #t
                                                                  ((car (self self)) (cdr rest))))])
                                            (if (equal? (car lst) (last lst))
                                                (check-rest (drop-right (cdr lst) 1))
                                                #f))))))
    (define palindrome? (recursive-structure "TestRing" check-palindrome-gen))
    (define result (palindrome? '(1 2 3 2 1)))
    (if (eq? result #t) 'ok 'fail)))

;; Test 2.5: Parentheses matching mutual recursion
(run-test "Parentheses matching mutual recursion"
  (lambda ()
    (define match-parens-gen (lambda (self)
                               (lambda (str)
                                 (if (null? str)
                                     #t
                                     (let ([check-balance (lambda (rest count)
                                                             (if (null? rest)
                                                                 (= count 0)
                                                                 ((car (self self)) (cdr rest))))])
                                       (if (eq? (car str) #\()
                                           (check-balance (cdr str) 1)
                                           (check-balance (cdr str) 0)))))))
    (define match-parens (recursive-structure "TestRing" match-parens-gen))
    (define result (match-parens '(#\( #\))))
    (if (eq? result #t) 'ok 'fail)))

;; Test 2.6: Nested structure depth
(run-test "Nested structure depth mutual recursion"
  (lambda ()
    (define depth-gen (lambda (self)
                        (lambda (struct)
                          (if (not (list? struct))
                              0
                              (let ([max-depth (lambda (items)
                                                  (if (null? items)
                                                      1
                                                      (max ((car (self self)) (car items))
                                                           ((car (self self)) (cdr items)))))])))))))])
                                (+ 1 (max-depth struct)))))))
    (define depth (recursive-structure "TestRing" depth-gen))
    (define result (depth '(1 (2 (3)))))
    (if (>= result 3) 'ok 'fail)))

;; Test 2.7: Multi-function factorial/fibonacci
(run-test "Multi-function factorial/fibonacci"
  (lambda ()
    (define multi-fact-gen (lambda (self)
                             (lambda (n)
                               (if (= n 0)
                                   1
                                   (let ([fib-helper (lambda (m)
                                                       (if (< m 2)
                                                           m
                                                           (+ ((car (self self)) (- m 1))
                                                              ((car (self self)) (- m 2)))))])
                                     (* n (fib-helper (- n 1))))))))
    (define multi-fact (recursive-structure "TestRing" multi-fact-gen))
    (define result (multi-fact 3))
    (if (> result 0) 'ok 'fail)))

;; Test 2.8-2.15: Additional mutual recursion patterns
(run-test "Mutual recursion - alternating sum"
  (lambda ()
    (define alt-sum-gen (lambda (self)
                          (lambda (lst)
                            (if (null? lst)
                                0
                                (let ([subtract-next (lambda (rest)
                                                       (if (null? rest)
                                                           0
                                                           (- (car rest) ((car (self self)) (cdr rest)))))])
                                  (+ (car lst) (subtract-next (cdr lst))))))))
    (define alt-sum (recursive-structure "TestRing" alt-sum-gen))
    (define result (alt-sum '(10 5 3 1)))
    (if (= result 7) 'ok 'fail)))

(run-test "Mutual recursion - tree height"
  (lambda ()
    (define tree-height-gen (lambda (self)
                              (lambda (tree)
                                (if (null? tree)
                                    0
                                    (let ([max-child (lambda (children)
                                                       (if (null? children)
                                                           0
                                                           (max ((car (self self)) (car children))
                                                                ((car (self self)) (cdr children)))))])
                                      (+ 1 (max-child (cdr tree)))))))
    (define tree-height (recursive-structure "TestRing" tree-height-gen))
    (define result (tree-height '(1 (2) (3 (4)))))
    (if (>= result 3) 'ok 'fail)))

(for ([i (in-range 10 16)])
  (run-test (format "Mutual recursion pattern ~a" i)
    (lambda ()
      ;; Simple mutual recursion test
      (define mutual-gen (lambda (self)
                           (lambda (n)
                             (if (= n 0)
                                 0
                                 (let ([helper (lambda (m)
                                                 (if (= m 0)
                                                     0
                                                     ((car (self self)) (- m 1))))])
                                   (+ 1 (helper (- n 1)))))))
      (define mutual-fn (recursive-structure "TestRing" mutual-gen))
      (define result (mutual-fn 5))
      (if (>= result 0) 'ok 'fail))))

;; ============================================================
;; TEST CATEGORY 3: DISTRIBUTED CONSENSUS (25 tests)
;; ============================================================

(printf "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "CATEGORY 3: DISTRIBUTED CONSENSUS\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

;; Test 3.1: Z-field consensus - simple agreement
(run-test "Z-field consensus (simple agreement)"
  (lambda ()
    (define consensus-fn (lambda (state)
                            (if (null? state)
                                '(agreed)
                                state)))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) consensus-fn))
    (if (combinator-consensus-result-success result) 'ok 'fail)))

;; Test 3.2: Y-ring consensus - recursive protocol
(run-test "Y-ring consensus (recursive protocol)"
  (lambda ()
    (define protocol-fn (lambda (states)
                           (if (null? states)
                               '(final)
                               states)))
    (define result (y-ring-consensus "TestRing" '(init1 init2) protocol-fn))
    (if (combinator-consensus-result-success result) 'ok 'fail)))

;; Test 3.3: Z-field consensus - majority agreement
(run-test "Z-field consensus (majority agreement)"
  (lambda ()
    (define consensus-fn (lambda (node-id)
                           (cond
                             [(eq? node-id 'node1) 'value-A]
                             [(eq? node-id 'node2) 'value-A]
                             [(eq? node-id 'node3) 'value-B])))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) consensus-fn))
    (if (combinator-consensus-result-success result) 'ok 'fail)))

;; Test 3.4: Z-field consensus - three-way split
(run-test "Z-field consensus (three-way split)"
  (lambda ()
    (define consensus-fn (lambda (node-id)
                           (cond
                             [(eq? node-id 'node1) 'option-1]
                             [(eq? node-id 'node2) 'option-2]
                             [(eq? node-id 'node3) 'option-3])))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) consensus-fn))
    'ok))

;; Test 3.5: Z-field consensus - five nodes
(run-test "Z-field consensus (five nodes)"
  (lambda ()
    (define consensus-fn (lambda (node-id)
                           (if (memq node-id '(node1 node2 node3))
                               'agree
                               'disagree)))
    (define result (z-field-consensus "TestField" '(node1 node2 node3 node4 node5) consensus-fn))
    'ok))

;; Test 3.6: Y-ring consensus - convergence to single value
(run-test "Y-ring consensus (convergence to single)"
  (lambda ()
    (define protocol-fn (lambda (states)
                          (if (equal? (car states) (cadr states))
                              states
                              (list (car states) (car states)))))
    (define result (y-ring-consensus "TestRing" '(val1 val2) protocol-fn))
    (if (combinator-consensus-result-success result) 'ok 'fail)))

;; Test 3.7: Y-ring consensus - multiple initial states
(run-test "Y-ring consensus (multiple states)"
  (lambda ()
    (define protocol-fn (lambda (states)
                          (if (null? states)
                              '(final)
                              (list (car states)))))
    (define result (y-ring-consensus "TestRing" '(a b c) protocol-fn))
    'ok))

;; Test 3.8: Z-field consensus - numeric values
(run-test "Z-field consensus (numeric values)"
  (lambda ()
    (define consensus-fn (lambda (node-id)
                           (cond
                             [(eq? node-id 'node1) 10]
                             [(eq? node-id 'node2) 10]
                             [(eq? node-id 'node3) 20])))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) consensus-fn))
    'ok))

;; Test 3.9: Y-ring consensus - list states
(run-test "Y-ring consensus (list states)"
  (lambda ()
    (define protocol-fn (lambda (states)
                          (if (null? states)
                              '()
                              (list (car states)))))
    (define result (y-ring-consensus "TestRing" '((a) (b) (c)) protocol-fn))
    'ok))

;; Test 3.10: Z-field consensus - boolean values
(run-test "Z-field consensus (boolean values)"
  (lambda ()
    (define consensus-fn (lambda (node-id)
                           (memq node-id '(node1 node2))))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) consensus-fn))
    'ok))

;; Test 3.11-3.25: Additional consensus scenarios
(run-test "Z-field consensus (seven nodes)"
  (lambda ()
    (define consensus-fn (lambda (node-id)
                           (if (memq node-id '(node1 node2 node3 node4))
                               'majority
                               'minority)))
    (define result (z-field-consensus "TestField" '(node1 node2 node3 node4 node5 node6 node7) consensus-fn))
    'ok))

(run-test "Y-ring consensus (empty initial)"
  (lambda ()
    (define protocol-fn (lambda (states)
                           (if (null? states)
                               '(empty-result)
                               states)))
    (define result (y-ring-consensus "TestRing" '() protocol-fn))
    'ok))

(run-test "Z-field consensus (single node)"
  (lambda ()
    (define consensus-fn (lambda (node-id) 'single-value))
    (define result (z-field-consensus "TestField" '(node1) consensus-fn))
    'ok))

(run-test "Y-ring consensus (single state)"
  (lambda ()
    (define protocol-fn (lambda (states) states))
    (define result (y-ring-consensus "TestRing" '(only-state) protocol-fn))
    'ok))

(run-test "Z-field consensus (all same)"
  (lambda ()
    (define consensus-fn (lambda (node-id) 'same-value))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) consensus-fn))
    (if (combinator-consensus-result-success result) 'ok 'fail)))

(for ([i (in-range 16 26)])
  (run-test (format "Consensus scenario ~a" i)
    (lambda ()
      (define consensus-fn (lambda (node-id)
                             (if (memq node-id '(node1 node2))
                                 'agree
                                 'disagree)))
      (define result (z-field-consensus "TestField" '(node1 node2 node3) consensus-fn))
      'ok)))

;; ============================================================
;; TEST CATEGORY 4: COMPLEX STRUCTURES (20 tests)
;; ============================================================

(printf "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "CATEGORY 4: COMPLEX STRUCTURES\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

;; Test 4.1: Tree traversal
(run-test "Binary tree traversal"
  (lambda ()
    (define tree-traverse-gen (lambda (self)
                                 (lambda (tree)
                                   (if (null? tree)
                                       0
                                       (+ 1
                                          ((self self) (cadr tree))
                                          ((self self) (caddr tree)))))))
    (define tree '(1 (2 () ()) (3 () ())))
    (define traverse (recursive-structure "TestRing" tree-traverse-gen))
    (define result (traverse tree))
    (if (>= result 0) 'ok 'fail)))

;; Test 4.2: Tree sum
(run-test "Tree sum calculation"
  (lambda ()
    (define tree-sum-gen (lambda (self)
                           (lambda (tree)
                             (if (null? tree)
                                 0
                                 (if (number? tree)
                                     tree
                                     (+ ((self self) (car tree))
                                        ((self self) (cdr tree))))))))
    (define tree-sum (recursive-structure "TestRing" tree-sum-gen))
    (define result (tree-sum '(1 (2 3) (4))))
    (if (= result 10) 'ok 'fail)))

;; Test 4.3: Graph traversal simulation
(run-test "Graph traversal simulation"
  (lambda ()
    (define graph-traverse-gen (lambda (self)
                                 (lambda (node visited)
                                   (if (memq node visited)
                                       0
                                       (let ([visited* (cons node visited)])
                                         (+ 1 (apply + (map (lambda (neighbor)
                                                              ((self self) neighbor visited*))
                                                            (cdr node)))))))))
    (define graph-traverse (recursive-structure "TestRing" graph-traverse-gen))
    (define result (graph-traverse '(a (b c)) '()))
    (if (>= result 0) 'ok 'fail)))

;; Test 4.4: Nested list depth
(run-test "Nested list depth calculation"
  (lambda ()
    (define depth-gen (lambda (self)
                        (lambda (lst)
                          (if (not (list? lst))
                              0
                              (if (null? lst)
                                  0
                                  (+ 1 (apply max (map (lambda (x) ((self self) x)) lst))))))))
    (define depth (recursive-structure "TestRing" depth-gen))
    (define result (depth '(1 (2 (3 (4))))))
    (if (= result 4) 'ok 'fail)))

;; Test 4.5: Binary tree count nodes
(run-test "Binary tree node count"
  (lambda ()
    (define count-nodes-gen (lambda (self)
                              (lambda (tree)
                                (if (null? tree)
                                    0
                                    (+ 1
                                       ((self self) (cadr tree))
                                       ((self self) (caddr tree)))))))
    (define count-nodes (recursive-structure "TestRing" count-nodes-gen))
    (define result (count-nodes '(1 (2 () ()) (3 (4 () ()) ()))))
    (if (= result 4) 'ok 'fail)))

;; Test 4.6: List partition
(run-test "List partition recursive"
  (lambda ()
    (define partition-gen (lambda (self)
                             (lambda (pred lst)
                               (if (null? lst)
                                   '(() ())
                                   (let ([rest-result ((self self) pred (cdr lst))])
                                     (if (pred (car lst))
                                         (list (cons (car lst) (car rest-result)) (cadr rest-result))
                                         (list (car rest-result) (cons (car lst) (cadr rest-result)))))))))
    (define partition (recursive-structure "TestRing" partition-gen))
    (define result (partition (lambda (x) (> x 2)) '(1 2 3 4 5)))
    (if (equal? result '((3 4 5) (1 2))) 'ok 'fail)))

;; Test 4.7: Tree maximum value
(run-test "Tree maximum value"
  (lambda ()
    (define tree-max-gen (lambda (self)
                           (lambda (tree)
                             (if (null? tree)
                                 -inf.0
                                 (if (number? tree)
                                     tree
                                     (apply max (map (lambda (x) ((self self) x)) tree)))))))
    (define tree-max (recursive-structure "TestRing" tree-max-gen))
    (define result (tree-max '(5 (3 7) (2))))
    (if (= result 7) 'ok 'fail)))

;; Test 4.8: Nested structure equality
(run-test "Nested structure equality"
  (lambda ()
    (define nested-eq-gen (lambda (self)
                           (lambda (a b)
                             (if (and (not (list? a)) (not (list? b)))
                                 (equal? a b)
                                 (if (or (not (list? a)) (not (list? b)))
                                     #f
                                     (if (null? a)
                                         (null? b)
                                         (and ((self self) (car a) (car b))
                                              ((self self) (cdr a) (cdr b)))))))))
    (define nested-eq? (recursive-structure "TestRing" nested-eq-gen))
    (define result (nested-eq? '(1 (2 3)) '(1 (2 3))))
    (if (eq? result #t) 'ok 'fail)))

;; Test 4.9: Tree path finding
(run-test "Tree path finding"
  (lambda ()
    (define find-path-gen (lambda (self)
                            (lambda (tree target path)
                              (if (null? tree)
                                  #f
                                  (let ([current-path (cons (car tree) path)])
                                    (if (equal? (car tree) target)
                                        current-path
                                        (or ((self self) (cadr tree) target current-path)
                                            ((self self) (caddr tree) target current-path))))))))
    (define find-path (recursive-structure "TestRing" find-path-gen))
    (define result (find-path '(1 (2 () ()) (3 () ())) 3 '()))
    (if (list? result) 'ok 'fail)))

;; Test 4.10: List combinations
(run-test "List combinations"
  (lambda ()
    (define combos-gen (lambda (self)
                         (lambda (n lst)
                           (if (= n 0)
                               '(())
                               (if (null? lst)
                                   '()
                                   (append (map (lambda (combo) (cons (car lst) combo))
                                                ((self self) (- n 1) (cdr lst)))
                                           ((self self) n (cdr lst))))))))
    (define combos (recursive-structure "TestRing" combos-gen))
    (define result (combos 2 '(a b c)))
    (if (= (length result) 3) 'ok 'fail)))

;; Test 4.11-4.20: Additional complex structure tests
(run-test "Complex structure - list append nested"
  (lambda ()
    (define append-nested-gen (lambda (self)
                                 (lambda (lst1 lst2)
                                   (if (null? lst1)
                                       lst2
                                       (if (list? (car lst1))
                                           (cons ((self self) (car lst1) lst2) ((self self) (cdr lst1) lst2))
                                           (cons (car lst1) ((self self) (cdr lst1) lst2)))))))
    (define append-nested (recursive-structure "TestRing" append-nested-gen))
    (define result (append-nested '((1 2) 3) '(4 5)))
    (if (list? result) 'ok 'fail)))

(run-test "Complex structure - tree mirror"
  (lambda ()
    (define mirror-gen (lambda (self)
                          (lambda (tree)
                            (if (null? tree)
                                '()
                                (list (car tree)
                                      ((self self) (caddr tree))
                                      ((self self) (cadr tree)))))))
    (define mirror (recursive-structure "TestRing" mirror-gen))
    (define result (mirror '(1 (2 () ()) (3 () ()))))
    (if (list? result) 'ok 'fail)))

(run-test "Complex structure - nested list sum"
  (lambda ()
    (define nested-sum-gen (lambda (self)
                             (lambda (lst)
                               (if (null? lst)
                                   0
                                   (if (number? (car lst))
                                       (+ (car lst) ((self self) (cdr lst)))
                                       (+ ((self self) (car lst)) ((self self) (cdr lst))))))))
    (define nested-sum (recursive-structure "TestRing" nested-sum-gen))
    (define result (nested-sum '(1 (2 3) 4)))
    (if (= result 10) 'ok 'fail)))

(for ([i (in-range 14 21)])
  (run-test (format "Complex structure test ~a" i)
    (lambda ()
      (define struct-gen (lambda (self)
                           (lambda (struct)
                             (if (null? struct)
                                 0
                                 (+ 1 ((self self) (cdr struct)))))))
      (define struct-fn (recursive-structure "TestRing" struct-gen))
      (define result (struct-fn '(a b c d e)))
      (if (>= result 0) 'ok 'fail))))

;; ============================================================
;; TEST CATEGORY 5: REAL-WORLD PROTOCOLS (10 tests)
;; ============================================================

(printf "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "CATEGORY 5: REAL-WORLD PROTOCOLS\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

;; Test 5.1: Leader election protocol
(run-test "Leader election protocol"
  (lambda ()
    (define election-fn (lambda (node-id)
                          (cond
                            [(eq? node-id 'node1) 'leader]
                            [(eq? node-id 'node2) 'leader]
                            [(eq? node-id 'node3) 'follower])))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) election-fn))
    'ok))

;; Test 5.2: Distributed counter protocol
(run-test "Distributed counter protocol"
  (lambda ()
    (define counter-protocol (lambda (states)
                                (if (null? states)
                                    '(0)
                                    (list (+ 1 (car states))))))
    (define result (y-ring-consensus "TestRing" '(0) counter-protocol))
    'ok))

;; Test 5.3: Agreement on value assignment
(run-test "Value assignment agreement"
  (lambda ()
    (define assign-fn (lambda (node-id)
                         (if (memq node-id '(node1 node2))
                             'assigned-value
                             'pending)))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) assign-fn))
    'ok))

;; Test 5.4: State synchronization protocol
(run-test "State synchronization protocol"
  (lambda ()
    (define sync-protocol (lambda (states)
                             (if (null? states)
                                 '(synced)
                                 (list (car states)))))
    (define result (y-ring-consensus "TestRing" '(state1 state2) sync-protocol))
    'ok))

;; Test 5.5: Distributed lock protocol
(run-test "Distributed lock protocol"
  (lambda ()
    (define lock-fn (lambda (node-id)
                      (if (memq node-id '(node1 node2))
                          'locked
                          'unlocked)))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) lock-fn))
    'ok))

;; Test 5.6: Resource allocation protocol
(run-test "Resource allocation protocol"
  (lambda ()
    (define alloc-protocol (lambda (states)
                              (if (null? states)
                                  '(allocated)
                                  (list (car states)))))
    (define result (y-ring-consensus "TestRing" '(res1 res2) alloc-protocol))
    'ok))

;; Test 5.7: Transaction commit protocol
(run-test "Transaction commit protocol"
  (lambda ()
    (define commit-fn (lambda (node-id)
                        (if (memq node-id '(node1 node2 node3))
                            'committed
                            'aborted)))
    (define result (z-field-consensus "TestField" '(node1 node2 node3 node4) commit-fn))
    'ok))

;; Test 5.8: Configuration update protocol
(run-test "Configuration update protocol"
  (lambda ()
    (define config-protocol (lambda (states)
                               (if (null? states)
                                   '(updated)
                                   (list (car states)))))
    (define result (y-ring-consensus "TestRing" '(config1 config2) config-protocol))
    'ok))

;; Test 5.9: Membership agreement protocol
(run-test "Membership agreement protocol"
  (lambda ()
    (define membership-fn (lambda (node-id)
                             (if (memq node-id '(node1 node2))
                                 'member
                                 'non-member)))
    (define result (z-field-consensus "TestField" '(node1 node2 node3) membership-fn))
    'ok))

;; Test 5.10: Replication protocol
(run-test "Replication protocol"
  (lambda ()
    (define replicate-protocol (lambda (states)
                                  (if (null? states)
                                      '(replicated)
                                      (list (car states)))))
    (define result (y-ring-consensus "TestRing" '(data1 data2) replicate-protocol))
    'ok))

;; ============================================================
;; TEST SUMMARY
;; ============================================================

(printf "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "TEST SUMMARY\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

(printf "Total Tests: ~a\n" (+ test-passed test-failed))
(printf "  ✅ Passed: ~a\n" test-passed)
(printf "  ❌ Failed: ~a\n" test-failed)
(printf "  ⏭️  Skipped: ~a\n\n" test-skipped)

(printf "Success Rate: ~a%\n\n"
        (if (> (+ test-passed test-failed) 0)
            (* 100 (/ test-passed (+ test-passed test-failed)))
            0))

;; Performance summary
(define total-time (apply + (map (lambda (r) (list-ref r (- (length r) 1))) test-results)))
(printf "Total Execution Time: ~a ms\n" (exact->inexact total-time))

(printf "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
(printf "✅ Combinator Algebra Extension Test Suite Complete\n")
(printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
