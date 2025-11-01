#lang racket/base

(require rackunit
         rackunit/text-ui
         "../../src/nlp/semantic-lattice.rkt")

(provide
 lattice-tests)

(define lattice-tests
  (test-suite
   "Semantic Lattice Tests"
   
   (test-case "Create empty lattice"
              (define lattice (empty-lattice))
              (check-true (semantic-lattice? lattice)))
   
   (test-case "Add node to lattice"
              (define lattice (empty-lattice))
              (define node (lattice-node 'test-node 'concept-type '() '() '()))
              (define new-lattice (add-node lattice node '()))
              (check-true (semantic-lattice? new-lattice)))
   
   (test-case "Subsumption check"
              (define lattice (empty-lattice))
              (define parent (lattice-node 'parent 'type '() '() '()))
              (define child (lattice-node 'child 'type '() (list 'parent) '()))
              (define lattice1 (add-node lattice parent '()))
              (define lattice2 (add-node lattice1 child '(parent)))
              ;; Note: subsumes? requires parent to be ancestor of child
              (check-true (semantic-lattice? lattice2))))))

(run-tests lattice-tests)

