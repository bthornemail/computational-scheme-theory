#lang racket/base

(require racket/match
         racket/string
         "../m-expression.rkt"
         "semantic-frame.rkt")

(provide
 map-causality-scheme
 map-pattern-scheme
 map-relation-scheme
 map-h1-computation)

;; ============================================================
;; DOMAIN-SPECIFIC MAPPINGS - Computational Scheme Theory
;; ============================================================

;; Map causality analysis to scheme operation
(define (map-causality-scheme frame)
  "Map causality analysis to scheme-theoretic operation"
  (define target-concept
    (ormap (lambda (c)
             (match c
               [`(entity ,type ,id)
                id]
               [else #f]))
           (semantic-frame-concepts frame)))
  
  (define change-type
    (ormap (lambda (m)
             (match m
               [`(,key ,value)
                (if (string? value)
                    (if (or (string-contains? value "drop")
                            (string-contains? value "decrease"))
                        "decrease"
                        "increase")
                    #f)]
               [else #f]))
           (semantic-frame-modifiers frame)))
  
  (m-expr 'analyzeCausalStructure
          (append (if target-concept (list target-concept) '())
                   (if change-type (list change-type) '())
                   '("relation: causal_influence"))))

;; Map pattern detection to topology operation
(define (map-pattern-scheme frame)
  "Map pattern detection to topology operation"
  (define target
    (ormap (lambda (c)
             (match c
               [`(entity ,type ,id)
                id]
               [else #f]))
           (semantic-frame-concepts frame)))
  
  (m-expr 'detectPatterns
          (if target (list target) '())))

;; Map relation analysis to fiber product operation
(define (map-relation-scheme frame)
  "Map relation analysis to scheme product operation"
  (define entities (filter (lambda (c) (eq? (car c) 'entity))
                            (semantic-frame-concepts frame)))
  
  (define entity-names
    (map (lambda (c)
           (match c
             [`(entity ,type ,id)
              id]
             [else ""]))
         entities))
  
  (m-expr 'analyzeRelations
          (append entity-names
                  '("computeHomSpaces: true"))))

;; Map H1 computation (main domain operation)
(define (map-h1-computation frame)
  "Map H1 computation query to M-expression"
  (define entities (filter (lambda (c) (eq? (car c) 'entity))
                            (semantic-frame-concepts frame)))
  
  (define program-name
    (ormap (lambda (c)
             (match c
               [`(entity "program" ,id)
                id]
               [else #f]))
           entities))
  
  (define parameters (semantic-frame-modifiers frame))
  
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

