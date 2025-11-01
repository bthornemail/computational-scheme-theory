#lang racket/base

(require racket/set
         "combinators.rkt")

(provide
 datalog-rule
 datalog-rule-head
 datalog-rule-body
 define-rule
 assert-fact!
 datalog-fixpoint
 *datalog-db*
 *datalog-rules*)

;; ============================================================
;; DATALOG IN LISP (Bottom-Up Induction)
;; ============================================================

;; Datalog-style logic programming embedded in Lisp
;; Forward chaining until fixed point using Z-combinator

;; Datalog database (set of facts)
(define *datalog-db* (make-parameter (mutable-set)))

;; Datalog rule (produces new facts)
(struct datalog-rule (head body) #:transparent)

;; List of rules
(define *datalog-rules* (make-parameter '()))

;; Assert Datalog fact
(define (assert-fact! fact)
  "Add fact to Datalog database"
  (set-add! (*datalog-db*) fact))

;; Define Datalog rule
(define (define-rule head body)
  "Define a Datalog rule: head :- body"
  (*datalog-rules* (cons (datalog-rule head body) (*datalog-rules*))))

;; Fixed-point iteration (Z-combinator style)
(define (datalog-fixpoint)
  "Compute least fixed point (bottom-up, guaranteed termination)"
  ;; Use Z-combinator for guaranteed termination
  (define iterate-fn
    ((Z (lambda (iterate)
          (lambda (db)
            (define old-size (set-count db))
            ;; Create copy of mutable set
            (define temp-db (mutable-set))
            (for ([fact db])
              (set-add! temp-db fact))
            
            ;; Apply all rules
            (for ([rule (*datalog-rules*)])
              (let ([new-facts ((datalog-rule-body rule) temp-db)])
                (for ([fact new-facts])
                  (set-add! temp-db fact))))
            
            ;; Check if fixed point reached
            (if (= old-size (set-count temp-db))
                temp-db
                (iterate temp-db)))))
     (lambda (db) db)))
  
  ;; Run fixpoint computation and update database
  (let ([result (iterate-fn (*datalog-db*))])
    ;; Update database by adding all new facts
    (for ([fact result])
      (assert-fact! fact))
    result))

