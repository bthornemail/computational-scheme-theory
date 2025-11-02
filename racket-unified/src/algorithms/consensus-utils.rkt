#lang racket/base

(provide
 node-state
 node-state?
 node-state-id
 node-state-state
 node-state-vote
 majority-value
 states-converged?)

;; Node state structure for consensus simulation
(struct node-state (id state vote) #:transparent)

;; Compute majority value from a list of states
(define (majority-value states)
  "Compute majority value from list of states (agreement)"
  (if (null? states)
      'no-consensus
      (let ([counts (make-hash)])
        ;; Count occurrences of each state
        (for-each (lambda (state)
                    (let ([val (if (node-state? state)
                                    (node-state-state state)
                                    state)])
                      (hash-set! counts val (+ 1 (hash-ref counts val 0)))))
                  states)
        ;; Find value with majority (> 50%)
        (let* ([total (length states)]
               [majority-threshold (/ total 2)])
          (let loop ([entries (hash->list counts)]
                     [best-value #f]
                     [best-count 0])
            (if (null? entries)
                (if (>= best-count majority-threshold)
                    best-value
                    'no-consensus)
                (let* ([entry (car entries)]
                       [value (car entry)]
                       [count (cdr entry)])
                  (if (> count best-count)
                      (loop (cdr entries) value count)
                      (loop (cdr entries) best-value best-count)))))))))

;; Check if states have converged (majority agreement)
(define (states-converged? states tolerance)
  "Check if states have converged to a consensus value"
  (let ([majority (majority-value states)])
    (if (eq? majority 'no-consensus)
        #f
        (let ([agreement-count
               (length (filter (lambda (s)
                                 (let ([val (if (node-state? s)
                                                 (node-state-state s)
                                                 s)])
                                   (equal? val majority)))
                               states))]
              [total (length states)])
          (>= agreement-count (* total tolerance))))))
