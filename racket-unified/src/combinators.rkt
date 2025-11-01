#lang racket/base

;; ============================================================
;; Y/Z Combinators for Fixed-Point Recursion
;; ============================================================

(provide Y Z)

;; Y-combinator for call-by-name (lazy evaluation)
;; Used for Prolog-style potentially infinite search
(define Y
  (lambda (f)
    ((lambda (x) (f (lambda args (apply (x x) args))))
     (lambda (x) (f (lambda args (apply (x x) args)))))))

;; Z-combinator for call-by-value (eager evaluation)
;; Used for Datalog-style guaranteed-terminating fixpoint
(define Z
  (lambda (f)
    ((lambda (x) (f (lambda args (apply (x x) args))))
     (lambda (x) (f (lambda args (apply (x x) args)))))))

