#lang racket/base

(require racket/match
         racket/set
         "algorithm1.rkt"
         "algorithm2.rkt"
         "incidence-structure.rkt"
         (only-in "../datalog-engine.rkt" assert-fact!))

(provide
 export-to-datalog
 ast-to-datalog-facts)

;; ============================================================
;; EXPORT TO DATALOG FACTS
;; ============================================================

;; Export AST to Datalog facts
(define (export-to-datalog ast bindings enhanced-scope-map incidence-struct)
  "Convert AST and incidence structure to Datalog facts"
  (define facts (ast-to-datalog-facts ast bindings enhanced-scope-map incidence-struct))
  
  ;; Add facts to Datalog database
  (for ([fact facts])
    (assert-fact! fact))
  
  facts)

;; Convert to Datalog facts
(define (ast-to-datalog-facts ast bindings enhanced-scope-map incidence-struct)
  "Generate Datalog facts from AST and incidence structure"
  (define facts '())
  
  ;; Points from incidence structure
  (when incidence-struct
    (for ([point (incidence-structure-points incidence-struct)])
      (let ([binding-id (incidence-point-binding-id (cdr point))])
        (set! facts (cons (list 'point binding-id) facts)))))
  
  ;; Hyperplanes from incidence structure
  (when incidence-struct
    (for ([hyperplane (incidence-structure-hyperplanes incidence-struct)])
      (let ([constraint-id (incidence-hyperplane-constraint-id (cdr hyperplane))])
        (set! facts (cons (list 'hyperplane constraint-id) facts)))))
  
  ;; Incidence relations
  (when incidence-struct
    (for ([key (hash-keys (incidence-structure-incidence-matrix incidence-struct))])
      (when (hash-ref (incidence-structure-incidence-matrix incidence-struct) key #f)
        (let ([point-id (car key)]
              [hyperplane-id (cdr key)])
          (set! facts (cons (list 'incident point-id hyperplane-id) facts))))))
  
  ;; Bindings from AST
  (for ([binding-id (in-set bindings)])
    (set! facts (cons (list 'binding binding-id) facts)))
  
  ;; Scope information from enhanced scope map
  (for ([(binding-id region) (in-hash enhanced-scope-map)])
    (cond
      [(visibility-region? region)
       (for ([scope (in-set (visibility-region-scopes region))])
         (set! facts (cons (list 'in_scope binding-id scope) facts)))]
      [(enhanced-visibility-region? region)
       (let ([scope-ids (enhanced-visibility-region-scope-ids region)])
         (for ([scope-id (in-set scope-ids)])
           (set! facts (cons (list 'in_scope binding-id scope-id) facts))))]))
  
  (reverse facts))

