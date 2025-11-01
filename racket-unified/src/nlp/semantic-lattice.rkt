#lang racket/base

(require racket/match
         racket/set
         racket/list)

(provide
 lattice-node
 lattice-node?
 lattice-node-id
 lattice-node-type
 lattice-node-properties
 lattice-node-parents
 lattice-node-children
 semantic-lattice
 semantic-lattice?
 semantic-lattice-nodes
 empty-lattice
 add-node
 find-meet
 find-join
 subsumes?
 lattice-node-add-parent
 lattice-node-add-child)

;; ============================================================
;; SEMANTIC LATTICE NETWORK - Complete Lattice (L, ≤)
;; ============================================================

;; Lattice node structure
(struct lattice-node (id type properties parents children) #:transparent)

;; Semantic lattice structure (set of nodes with partial order)
(struct semantic-lattice (nodes) #:transparent)

;; Create empty lattice
(define (empty-lattice)
  "Create empty semantic lattice"
  (semantic-lattice (set)))

;; Add node to lattice
(define (add-node lattice node parents)
  "Add node to lattice with parent relationships"
  (define existing-nodes (semantic-lattice-nodes lattice))
  
  ;; Add parent relationships
  (define node-with-parents
    (foldl (lambda (parent-id n)
             (if (set-member? existing-nodes (lattice-node parent-id #f '() '() '()))
                 (lattice-node-add-parent n parent-id)
                 n))
           node
           parents))
  
  ;; Update parent nodes to include this as child
  (define updated-nodes
    (foldl (lambda (n acc)
             (if (member (lattice-node-id n) parents)
                 (set-add acc (lattice-node-add-child n (lattice-node-id node)))
                 acc))
           (set-add existing-nodes node-with-parents)
           (set->list existing-nodes)))
  
  (semantic-lattice updated-nodes))

;; Helper to add parent to node
(define (lattice-node-add-parent node parent-id)
  "Add parent to lattice node"
  (struct-copy lattice-node node
               [parents (cons parent-id (lattice-node-parents node))]))

;; Helper to add child to node
(define (lattice-node-add-child node child-id)
  "Add child to lattice node"
  (struct-copy lattice-node node
               [children (cons child-id (lattice-node-children node))]))

;; Find meet (greatest lower bound)
(define (find-meet lattice node-a-id node-b-id)
  "Find greatest lower bound (meet) of two nodes"
  (define nodes (semantic-lattice-nodes lattice))
  (define node-a (find-node-by-id nodes node-a-id))
  (define node-b (find-node-by-id nodes node-b-id))
  
  (if (or (not node-a) (not node-b))
      #f
      (let ([ancestors-a (get-ancestors nodes node-a-id)]
            [ancestors-b (get-ancestors nodes node-b-id)])
        (define common-ancestors (set-intersect ancestors-a ancestors-b))
        (if (set-empty? common-ancestors)
            #f
            ;; Find most specific common ancestor
            (find-most-specific-ancestor nodes common-ancestors)))))

;; Find join (least upper bound)
(define (find-join lattice node-a-id node-b-id)
  "Find least upper bound (join) of two nodes"
  (define nodes (semantic-lattice-nodes lattice))
  (define node-a (find-node-by-id nodes node-a-id))
  (define node-b (find-node-by-id nodes node-b-id))
  
  (if (or (not node-a) (not node-b))
      #f
      (let ([descendants-a (get-descendants nodes node-a-id)]
            [descendants-b (get-descendants nodes node-b-id)])
        (define common-descendants (set-intersect descendants-a descendants-b))
        (if (set-empty? common-descendants)
            #f
            ;; Find most general common descendant
            (find-most-general-descendant nodes common-descendants)))))

;; Check if node-a subsumes node-b (a ≤ b)
(define (subsumes? lattice node-a-id node-b-id)
  "Check if node-a subsumes node-b (a ≤ b)"
  (define nodes (semantic-lattice-nodes lattice))
  (define path (find-subsumption-path nodes node-a-id node-b-id))
  (not (null? path)))

;; Find subsumption path (transitive closure) - internal helper
(define (find-subsumption-path nodes from-id to-id)
  "Find path from from-id to to-id in lattice"
  (letrec ([dfs (lambda (current-id target-id path visited-so-far)
                  (if (eq? current-id target-id)
                      (reverse (cons current-id path))
                      (if (set-member? visited-so-far current-id)
                          #f
                          (let ([visited-new (set-add visited-so-far current-id)])
                            (let ([node (find-node-by-id nodes current-id)])
                              (if node
                                  (ormap (lambda (child-id)
                                           (dfs child-id target-id (cons current-id path) visited-new))
                                         (lattice-node-children node))
                                  #f))))))])
    (dfs from-id to-id '() (set))))

;; Helper: Get all ancestors of a node
(define (get-ancestors nodes node-id)
  "Get all ancestor nodes (transitive closure of parents)"
  (letrec ([visited (set)]
           [collect (lambda (node-id visited-so-far)
                      (if (set-member? visited-so-far node-id)
                          (set)
                          (let* ([visited-new (set-add visited-so-far node-id)]
                                 [node (find-node-by-id nodes node-id)])
                            (if node
                                (let ([parent-set (list->set (lattice-node-parents node))])
                                  (foldl (lambda (parent-id acc)
                                           (set-union acc (collect parent-id visited-new) (set parent-id)))
                                         parent-set
                                         (lattice-node-parents node)))
                                (set)))))])
    (collect node-id visited)))

;; Helper: Get all descendants of a node
(define (get-descendants nodes node-id)
  "Get all descendant nodes (transitive closure of children)"
  (letrec ([collect (lambda (node-id visited-so-far)
                      (if (set-member? visited-so-far node-id)
                          (set)
                          (let* ([visited-new (set-add visited-so-far node-id)]
                                 [node (find-node-by-id nodes node-id)])
                            (if node
                                (let ([child-set (list->set (lattice-node-children node))])
                                  (foldl (lambda (child-id acc)
                                           (set-union acc (collect child-id visited-new) (set child-id)))
                                         child-set
                                         (lattice-node-children node)))
                                (set)))))])
    (collect node-id (set))))

;; Helper: Find node by ID
(define (find-node-by-id nodes node-id)
  "Find lattice node by ID"
  (for/first ([node (in-set nodes)]
              #:when (eq? (lattice-node-id node) node-id))
    node))

;; Helper: Find most specific ancestor
(define (find-most-specific-ancestor nodes ancestor-ids)
  "Find most specific (lowest in hierarchy) ancestor"
  (define candidates (map (lambda (id) (find-node-by-id nodes id))
                          (set->list ancestor-ids)))
  (for/first ([candidate candidates]
              #:when (and candidate
                          (for/and ([other candidates]
                                    #:when (not (eq? candidate other)))
                            (subsumes?-helper nodes candidate other))))
    (lattice-node-id candidate)))

;; Helper: Find most general descendant
(define (find-most-general-descendant nodes descendant-ids)
  "Find most general (highest in hierarchy) descendant"
  (define candidates (map (lambda (id) (find-node-by-id nodes id))
                          (set->list descendant-ids)))
  (for/first ([candidate candidates]
              #:when (and candidate
                          (for/and ([other candidates]
                                    #:when (not (eq? candidate other)))
                            (not (subsumes?-helper nodes candidate other)))))
    (lattice-node-id candidate)))

;; Helper: Check subsumption between nodes
(define (subsumes?-helper nodes node-a node-b)
  "Helper to check if node-a subsumes node-b"
  (member (lattice-node-id node-a)
          (lattice-node-parents node-b)))

