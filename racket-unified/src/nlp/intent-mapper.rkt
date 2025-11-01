#lang racket/base

(require racket/match
         racket/string
         "../m-expression.rkt"
         "semantic-frame.rkt")

(provide
 map-to-m-expression
 classify-operation
 validate-type-constraints)

;; ============================================================
;; INTENT MAPPER - Functor: Semantic Frame → M-Expression
;; ============================================================

;; Map enhanced frame to M-expression
(define (map-to-m-expression enhanced-frame)
  "Map enhanced semantic frame to M-expression"
  (define frame (enhanced-frame-frame enhanced-frame))
  (define operation-type (classify-operation frame))
  
  (if (not operation-type)
      (error "Cannot classify operation type")
      (case operation-type
        [('compute-h1)
         (map-compute-h1 frame)]
        [('compute-vg)
         (map-compute-vg frame)]
        [('validate-hypothesis)
         (map-validate-hypothesis frame)]
        [('analyze-patterns)
         (map-analyze-patterns frame)]
        [('compare-metrics)
         (map-compare-metrics frame)]
        [else
         (error "Unknown operation type:" operation-type)])))

;; Classify operation from semantic frame
(define (classify-operation frame)
  "Classify operation type from semantic frame"
  (define intent-type (semantic-frame-intent-type frame))
  (define objects (filter (lambda (c) (eq? (car c) 'object))
                          (semantic-frame-concepts frame)))
  (define object-values (map cadr objects))
  (define object-values-lower (map string-downcase object-values))
  
  (cond
    [(and intent-type (string=? (string-downcase intent-type) "compute"))
     (cond
       [(ormap (lambda (v) (string-prefix? (string-downcase v) "h1")) object-values)
        'compute-h1]
       [(ormap (lambda (v) (string-prefix? (string-downcase v) "h¹")) object-values)
        'compute-h1]
       [(ormap (lambda (v) (member (string-downcase v) '("v(g)" "vg"))) object-values)
        'compute-vg]
       [else
        'compute-operation])]
    [(and intent-type (string=? (string-downcase intent-type) "validate"))
     'validate-hypothesis]
    [(and intent-type (string=? (string-downcase intent-type) "analyze"))
     'analyze-patterns]
    [(and intent-type (string=? (string-downcase intent-type) "compare"))
     'compare-metrics]
    [else
     #f]))

;; Validate type constraints
(define (validate-type-constraints frame)
  "Validate type constraints in semantic frame"
  (define concepts (semantic-frame-concepts frame))
  (define intent-type (semantic-frame-intent-type frame))
  
  ;; Must have action verb
  (define has-verb (ormap (lambda (c) (eq? (car c) 'action-verb)) concepts))
  
  ;; Must have object for compute operations
  (define has-object
    (if (and intent-type (eq? intent-type "compute"))
        (ormap (lambda (c) (eq? (car c) 'object)) concepts)
        #t))
  
  (and has-verb has-object))

;; Map to compute H1 operation
(define (map-compute-h1 frame)
  "Map frame to compute H1 M-expression"
  (define entities (filter (lambda (c) (eq? (car c) 'entity))
                            (semantic-frame-concepts frame)))
  (define parameters (semantic-frame-modifiers frame))
  
  (define program-name
    (ormap (lambda (c)
             (match c
               [`(entity "program" ,id)
                id]
               [else #f]))
           entities))
  
  (define k-value
    (ormap (lambda (m)
             (match m
               [`(parameter ,param)
                (if (string-prefix? (format "~a" param) "k=")
                    (string->number (substring (format "~a" param) 2))
                    #f)]
               [else #f]))
           parameters))
  
  (define args
    (append (if program-name (list program-name) '())
            (if k-value (list (format "k=~a" k-value)) '())))
  
  (m-expr 'computeH1 args))

;; Map to compute V(G) operation
(define (map-compute-vg frame)
  "Map frame to compute V(G) M-expression"
  (define entities (filter (lambda (c) (eq? (car c) 'entity))
                            (semantic-frame-concepts frame)))
  
  (define program-name
    (ormap (lambda (c)
             (match c
               [`(entity "program" ,id)
                id]
               [else #f]))
           entities))
  
  (m-expr 'computeVG (if program-name (list program-name) '())))

;; Map to validate hypothesis operation
(define (map-validate-hypothesis frame)
  "Map frame to validate hypothesis M-expression"
  (m-expr 'validateHypothesis '()))

;; Map to analyze patterns operation
(define (map-analyze-patterns frame)
  "Map frame to analyze patterns M-expression"
  (m-expr 'analyzePatterns '()))

;; Map to compare metrics operation
(define (map-compare-metrics frame)
  "Map frame to compare metrics M-expression"
  (m-expr 'compareMetrics '()))

