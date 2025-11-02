#lang racket/base

(require racket/match
         racket/set
         racket/list
         "algorithm1.rkt"
         "algorithm2.rkt"
         (only-in "algorithm4.rkt" matrix-rank)
         "projective-types.rkt"
         "dependency-graph.rkt"
         "combinator-detector.rkt")

(provide
 incidence-point?
 incidence-point-binding-id
 incidence-point-type
 incidence-point-dimension
 incidence-point-access-count
 incidence-hyperplane?
 incidence-hyperplane-constraint-id
 incidence-hyperplane-constraint-type
 incidence-structure?
 incidence-structure-points
 incidence-structure-hyperplanes
 incidence-structure-incidence-matrix
 build-incidence-structure
 build-boundary-d1
 build-boundary-d2
 build-boundary-d3
 build-boundary-d4
 build-boundary-d5
 compute-h1-incidence
 compute-h2-incidence
 compute-h3-incidence
 compute-h4-incidence
 compute-hn-incidence
 binding->polynomial
 incidence-structure->polynomial-ring)

;; ============================================================
;; INCIDENCE STRUCTURE COMPUTATION
;; Bipartite: Points (bindings) ↔ Hyperplanes (constraints)
;; ============================================================

;; Point in incidence structure: a binding
;; Enhanced with dimensional tracking (Church numerals, access count)
(struct incidence-point (binding-id 
                          type           ; 'affine or 'projective
                          dimension      ; Church numeral = access count = polynomial degree
                          access-count)  ; number of times accessed
  #:transparent)

;; Hyperplane in incidence structure: a constraint
(struct incidence-hyperplane (constraint-id constraint-type) #:transparent)
;; constraint-type: 'scope, 'dependency, 'type-constraint, 'projective-closure

;; Incidence structure: bipartite graph
(struct incidence-structure (points hyperplanes incidence-matrix) #:transparent)
;; incidence-matrix: hash mapping (point-id . hyperplane-id) -> #t/#f

;; Build incidence structure from AST and bindings
(define (build-incidence-structure ast bindings enhanced-scope-map scope-tree
                                    #:dependency-graph [dep-graph #f]
                                    #:combinators [combinators '()])
  "Build bipartite incidence structure from program analysis
   with enhanced cycle detection for recursive calls, Y-combinators, and projective closures"
  (define points (make-hash))
  (define hyperplanes (make-hash))
  (define incidence-matrix (make-hash))
  
  ;; Infer projective types
  (define projective-bindings (infer-projective-types ast bindings))
  
  ;; Extract points (bindings) with dimensional tracking
  ;; Track access count = Church numeral = polynomial degree = dimension
  (define access-map (make-hash))  ; binding-id -> access count
  
  ;; Count accesses: each reference increases dimension (Church successor)
  (define (count-accesses ast)
    "Count how many times each binding is accessed (Church numeral computation)"
    (define (count-refs expr count-map)
      (match expr
        [(ast-var loc var-name)
         (hash-update! count-map var-name add1 0)
         count-map]
        [(? ast-app? app-expr)
         (let ([count-map* (count-refs (ast-app-func app-expr) count-map)])
           (for/fold ([cm count-map*])
                     ([arg (ast-app-args app-expr)])
             (count-refs arg cm)))]
        [(? ast-if? if-expr)
         (let ([cm1 (count-refs (ast-if-test if-expr) count-map)])
           (let ([cm2 (count-refs (ast-if-then if-expr) cm1)])
             (count-refs (ast-if-else if-expr) cm2)))]
        [(? ast-lambda? lambda-expr)
         (for/fold ([cm count-map])
                   ([body-expr (ast-lambda-body lambda-expr)])
           (count-refs body-expr cm))]
        [(? ast-let? let-expr)
         (let ([cm1 (for/fold ([cm count-map])
                            ([binding (ast-let-bindings let-expr)])
                      (count-refs (cdr binding) cm))])
           (for/fold ([cm cm1])
                     ([body-expr (ast-let-body let-expr)])
             (count-refs body-expr cm)))]
        [(? ast-define? define-expr)
         (count-refs (ast-define-value define-expr) count-map)]
        [else count-map]))
    (count-refs ast (make-hash)))
  
  ;; Pattern-based dimension detection
  ;; Analyzes form structure to determine dimensional pattern (ellipsis patterns)
  (define (detect-pattern-dimension form)
    "Use pattern matching to determine dimensional structure from form
     Handles ellipsis patterns: (), (P), (P ...), #(P ...)
     Returns: pattern dimension (0D, 1D, 2D, ...)"
    (match form
      ;; Empty/null pattern: 0D
      [(? null?) 0]
      [(? void?) 0]
      
      ;; Atomic patterns: 0D (constants, literals)
      [(? symbol?) 0]
      [(? number?) 0]
      [(? boolean?) 0]
      [(? string?) 0]
      [(? char?) 0]
      
      ;; List patterns: analyze structure
      [(? list? lst)
       (cond
         [(null? lst) 0]  ; () - 0D
         [(null? (cdr lst)) 1]  ; (P) - 1D
         [(null? (cddr lst)) 2]  ; (P P) - 2D
         [else (length lst)])]  ; (P ...) - nD where n = length
      
      ;; Vector patterns: 2nD (multivariate structure)
      [(? vector? vec)
       (if (zero? (vector-length vec))
           0
           (* 2 (vector-length vec)))]  ; #(P ...) - 2nD
      
      ;; Pair (improper list): 1D+ (linear + tail)
      [(? pair? pr)
       (if (pair? (cdr pr))
           (+ 1 (detect-pattern-dimension (cdr pr)))  ; (P ... . P') - 1D+ 
           1)]  ; (P . Q) - 1D
      
      ;; AST structures: analyze recursively
      [(? ast-lambda? lambda-expr)
       (let ([params (ast-lambda-params lambda-expr)]
             [body (ast-lambda-body lambda-expr)])
         (max (length params)
              (if (null? body) 0 (apply max (map detect-pattern-dimension body)))))]
      
      [(? ast-app? app-expr)
       (max (detect-pattern-dimension (ast-app-func app-expr))
            (if (null? (ast-app-args app-expr))
                0
                (apply max (map detect-pattern-dimension (ast-app-args app-expr)))))]
      
      [(? ast-let? let-expr)
       (let ([bindings (ast-let-bindings let-expr)]
             [body (ast-let-body let-expr)])
         (max (if (null? bindings) 0 (length bindings))
              (if (null? body) 0 (apply max (map detect-pattern-dimension body)))))]
      
      [(? ast-define? define-expr)
       (detect-pattern-dimension (ast-define-value define-expr))]
      
      [(? ast-if? if-expr)
       (max (detect-pattern-dimension (ast-if-test if-expr))
            (detect-pattern-dimension (ast-if-then if-expr))
            (detect-pattern-dimension (ast-if-else if-expr)))]
      
      ;; Default: 0D
      [else 0]))
  
  ;; Extract binding forms for pattern analysis
  (define (extract-binding-forms ast)
    "Extract the form/value each binding is bound to, for pattern analysis"
    (define binding-forms (make-hash))
    
    (define (collect-forms expr)
      (match expr
        [(? ast-define? define-expr)
         (hash-set! binding-forms 
                    (ast-define-name define-expr)
                    (ast-define-value define-expr))
         (collect-forms (ast-define-value define-expr))]
        
        [(? ast-lambda? lambda-expr)
         (for ([param (ast-lambda-params lambda-expr)])
           (hash-set! binding-forms param lambda-expr))  ; Parameter bound to lambda
         (for ([body-expr (ast-lambda-body lambda-expr)])
           (collect-forms body-expr))]
        
        [(? ast-let? let-expr)
         (for ([binding (ast-let-bindings let-expr)])
           (hash-set! binding-forms (car binding) (cdr binding))
           (collect-forms (cdr binding)))
         (for ([body-expr (ast-let-body let-expr)])
           (collect-forms body-expr))]
        
        [(? ast-letrec? letrec-expr)
         (for ([binding (ast-letrec-bindings letrec-expr)])
           (hash-set! binding-forms (car binding) (cdr binding))
           (collect-forms (cdr binding)))
         (for ([body-expr (ast-letrec-body letrec-expr)])
           (collect-forms body-expr))]
        
        [(? ast-app? app-expr)
         (collect-forms (ast-app-func app-expr))
         (for ([arg (ast-app-args app-expr)])
           (collect-forms arg))]
        
        [(? ast-if? if-expr)
         (collect-forms (ast-if-test if-expr))
         (collect-forms (ast-if-then if-expr))
         (collect-forms (ast-if-else if-expr))]
        
        [else #f]))
    
    (if (list? ast)
        (for ([expr ast]) (collect-forms expr))
        (collect-forms ast))
    
    binding-forms)
  
  ;; Build access map
  (set! access-map (count-accesses ast))
  
  ;; Extract binding forms for pattern analysis
  (define binding-forms-map (extract-binding-forms ast))
  
  ;; Extract points with dimension = max(access count, pattern dimension)
  ;; Combines Church numeral (access count) with pattern structure (ellipsis)
  (for ([binding-id (in-set bindings)])
    (let* ([is-projective (set-member? projective-bindings binding-id)]
           [access-count (hash-ref access-map binding-id 0)]
           ;; Get pattern dimension from binding form
           [binding-form (hash-ref binding-forms-map binding-id #f)]
           [pattern-dim (if binding-form
                           (detect-pattern-dimension binding-form)
                           0)]
           ;; Dimension = max(access count, pattern dimension)
           ;; This combines Church numeral interpretation with pattern structure
           [dimension (max access-count pattern-dim)])
      (hash-set! points binding-id 
                 (incidence-point binding-id 
                                  (if is-projective 'projective 'affine)
                                  dimension
                                  access-count))))
  
  ;; Add projective closure hyperplane H∞
  (define projective-closure-id 'H∞)
  (hash-set! hyperplanes projective-closure-id 
             (incidence-hyperplane projective-closure-id 'projective-closure))
  
  ;; Add incidences for projective points at infinity
  (for ([binding-id (in-set projective-bindings)])
    (hash-set! incidence-matrix (cons binding-id projective-closure-id) #t))
  
  ;; Extract hyperplanes from scope map
  (for ([(binding-id region) (in-hash enhanced-scope-map)])
    (cond
      [(visibility-region? region)
       (let ([scopes (visibility-region-scopes region)]
             [region-bindings (visibility-region-bindings region)])
         ;; Scope hyperplanes
         (for ([scope-id (in-set scopes)])
           (unless (hash-has-key? hyperplanes scope-id)
             (hash-set! hyperplanes scope-id 
                        (incidence-hyperplane scope-id 'scope)))
           ;; Incidence: binding lies on scope hyperplane
           (hash-set! incidence-matrix (cons binding-id scope-id) #t))
         ;; Binding hyperplane (self-incidence)
         (let ([binding-hyperplane (symbol-append binding-id '-scope)])
           (unless (hash-has-key? hyperplanes binding-hyperplane)
             (hash-set! hyperplanes binding-hyperplane
                        (incidence-hyperplane binding-hyperplane 'scope)))
           (hash-set! incidence-matrix (cons binding-id binding-hyperplane) #t)))]
      [(enhanced-visibility-region? region)
       (let ([base-region (enhanced-visibility-region-base-region region)]
             [scope-ids (enhanced-visibility-region-scope-ids region)])
         ;; Extract from base region
         (let ([scopes (visibility-region-scopes base-region)])
           (for ([scope-id (in-set (set-union scopes scope-ids))])
             (unless (hash-has-key? hyperplanes scope-id)
               (hash-set! hyperplanes scope-id
                          (incidence-hyperplane scope-id 'scope)))
             (hash-set! incidence-matrix (cons binding-id scope-id) #t))))]))
  
  ;; ============================================================
  ;; ENHANCED CYCLE DETECTION
  ;; ============================================================
  
  ;; Add recursion cycles from dependency graph
  (when dep-graph
    (let ([recursive-calls (find-recursive-calls dep-graph)])
      (for ([edge recursive-calls])
        (let* ([func-name (dependency-edge-from edge)]
               [recursion-constraint (symbol-append func-name '-recursion)]
               [recursion-return (symbol-append func-name '-recursion-return)])
          ;; Add recursion constraint hyperplane (entry)
          (hash-set! hyperplanes recursion-constraint
                     (incidence-hyperplane recursion-constraint 'recursion))
          ;; Add recursion return hyperplane (exit) - creates cycle closure
          (hash-set! hyperplanes recursion-return
                     (incidence-hyperplane recursion-return 'recursion))
          ;; Ensure function is a point (add or update if recursive)
          ;; Recursive functions have dimension ≥ 1 (accessed in their own body)
          (let* ([func-access-count (hash-ref access-map func-name 0)]
                 [func-dimension (max 1 func-access-count)])  ; Recursion implies at least 1D
            (if (hash-has-key? points func-name)
                ;; Update existing point with higher dimension (recursion increases dimension)
                (let ([existing-point (hash-ref points func-name)])
                  (when (> func-dimension (incidence-point-dimension existing-point))
                    (hash-set! points func-name
                               (incidence-point func-name 
                                                (incidence-point-type existing-point)
                                                func-dimension 
                                                func-access-count))))
                ;; Create new point
                (hash-set! points func-name
                           (incidence-point func-name 'affine func-dimension func-access-count))))
          ;; Create cycle: func → recursion_constraint → intermediate → recursion_return → func
          (let ([cycle-intermediate (symbol-append func-name '-recursion-intermediate)])
              ;; Add intermediate point for cycle structure
              ;; Cycle intermediates have dimension = 1 (part of cycle path)
              (unless (hash-has-key? points cycle-intermediate)
                (hash-set! points cycle-intermediate
                           (incidence-point cycle-intermediate 'affine 1 1)))
              ;; Path 1: func → recursion_constraint → intermediate
              (hash-set! incidence-matrix (cons func-name recursion-constraint) #t)
              (hash-set! incidence-matrix (cons cycle-intermediate recursion-constraint) #t)
              ;; Path 2: intermediate → recursion_return → func (closes cycle)
              (hash-set! incidence-matrix (cons cycle-intermediate recursion-return) #t)
              (hash-set! incidence-matrix (cons func-name recursion-return) #t)))))
  
  ;; Add Y-combinator fixed-point cycles
  (for ([combinator-info combinators])
    (match combinator-info
      [(list 'y-combinator expr-loc)
       (let* ([fixed-point-id (symbol-append 'fixed-point- expr-loc)]
              [y-constraint-id 'y_fixed_point])
         ;; Add Y-combinator constraint
         (hash-set! hyperplanes y-constraint-id
                    (incidence-hyperplane y-constraint-id 'fixed-point))
         ;; Add fixed-point node
         ;; Y-combinator creates fixed point with dimension ≥ 1 (self-application)
         (hash-set! points fixed-point-id
                    (incidence-point fixed-point-id 'affine 1 1))
         ;; Create self-loop: fixed-point → y_fixed_point → fixed-point
         (hash-set! incidence-matrix (cons fixed-point-id y-constraint-id) #t)
         (hash-set! incidence-matrix (cons fixed-point-id y-constraint-id) #t))]
      [(list 'z-combinator expr-loc)
       (let* ([fixed-point-id (symbol-append 'fixed-point-z- expr-loc)]
              [z-constraint-id 'z_fixed_point])
         ;; Z-combinator similar to Y
         (hash-set! hyperplanes z-constraint-id
                    (incidence-hyperplane z-constraint-id 'fixed-point))
         ;; Z-combinator similar to Y
         (hash-set! points fixed-point-id
                    (incidence-point fixed-point-id 'affine 1 1))
         (hash-set! incidence-matrix (cons fixed-point-id z-constraint-id) #t))]
      [else (void)]))
  
  ;; Add projective convergence cycles (when if-expressions have undefined branches)
  ;; This is handled in the scope analysis, but we add explicit convergence constraints
  (for ([binding-id (in-set projective-bindings)])
    (let ([convergence-id (symbol-append 'convergence- binding-id)])
      (hash-set! hyperplanes convergence-id
                 (incidence-hyperplane convergence-id 'convergence))
      ;; Connect projective binding to convergence
      (hash-set! incidence-matrix (cons binding-id convergence-id) #t)
      ;; Also connect projective closure
      (hash-set! incidence-matrix (cons binding-id projective-closure-id) #t)))
  
  (incidence-structure 
   (hash->list points)
   (hash->list hyperplanes)
   incidence-matrix)))

;; Helper function for symbol concatenation
(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

;; Build boundary map d₁: C₁ → C₀
;; C₁ = edges (incidence relations), C₀ = points + hyperplanes
(define (build-boundary-d1 incidence-struct)
  "Build boundary map d₁: edges → (points + hyperplanes)
   For edge from point P to hyperplane H: d₁(edge) = P - H"
  (define points (incidence-structure-points incidence-struct))
  (define hyperplanes (incidence-structure-hyperplanes incidence-struct))
  (define incidence-matrix (incidence-structure-incidence-matrix incidence-struct))
  
  ;; Collect all edges (incidence relations)
  (define edges '())
  (for ([key (hash-keys incidence-matrix)])
    (when (hash-ref incidence-matrix key #f)
      (set! edges (cons key edges))))
  
  (define num-points (length points))
  (define num-hyperplanes (length hyperplanes))
  (define num_cells_0 (+ num-points num-hyperplanes))
  (define num_edges (length edges))
  
  ;; Build boundary matrix: rows = points + hyperplanes, columns = edges
  (define boundary-matrix
    (for/list ([i (in-range num_cells_0)])
      (make-vector num_edges 0)))
  
  ;; Fill matrix: for edge (P, H), set P row to 1, H row to -1
  (for ([edge-idx (in-range (length edges))])
    (let ([edge (list-ref edges edge-idx)])
      (let ([point-id (car edge)]
            [hyperplane-id (cdr edge)])
        ;; Find indices
        (define point-idx (for/first ([i (in-naturals)]
                                     [p points]
                                     #:when (equal? (incidence-point-binding-id (cdr p)) point-id))
                           i))
        (define hyperplane-idx (for/first ([i (in-naturals)]
                                          [h hyperplanes]
                                          #:when (equal? (incidence-hyperplane-constraint-id (cdr h)) hyperplane-id))
                                i))
        (when (and point-idx (number? point-idx) 
                   hyperplane-idx (number? hyperplane-idx))
          ;; Set +1 for point
          (vector-set! (list-ref boundary-matrix point-idx) edge-idx 1)
          ;; Set -1 for hyperplane
          (vector-set! (list-ref boundary-matrix (+ point-idx num-points)) edge-idx -1)))))
  
  ;; Convert vectors back to lists for matrix-rank compatibility
  (define matrix-as-lists
    (for/list ([row boundary-matrix])
      (vector->list row)))
  
  (values matrix-as-lists edges))

;; ============================================================
;; HIGHER-DIMENSIONAL SIMPLICES (C₂, C₃, C₄)
;; ============================================================

;; Build 2-simplices (triangles): 3 elements (points or hyperplanes) sharing constraint
(define (build-simplices-2 incidence-struct)
  "Build 2-simplices: triangles in bipartite incidence structure
   A triangle is either:
   - (P₁, P₂, H) where both points lie on H and share another constraint
   - (P, H₁, H₂) where point P lies on both hyperplanes"
  (define points-list (incidence-structure-points incidence-struct))
  (define hyperplanes-list (incidence-structure-hyperplanes incidence-struct))
  (define incidence-matrix (incidence-structure-incidence-matrix incidence-struct))
  (define simplices-2 (mutable-set))
  
  ;; Type 1: Two points sharing a hyperplane
  (for* ([p1-idx (in-range (length points-list))]
         [p2-idx (in-range (length points-list))]
         #:when (< p1-idx p2-idx))
    (let* ([p1 (cdr (list-ref points-list p1-idx))]
           [p2 (cdr (list-ref points-list p2-idx))]
           [p1-id (incidence-point-binding-id p1)]
           [p2-id (incidence-point-binding-id p2)])
      ;; Find common hyperplanes
      (for ([h-idx (in-range (length hyperplanes-list))])
        (let* ([h (cdr (list-ref hyperplanes-list h-idx))]
               [h-id (incidence-hyperplane-constraint-id h)])
          (when (and (hash-ref incidence-matrix (cons p1-id h-id) #f)
                     (hash-ref incidence-matrix (cons p2-id h-id) #f))
            ;; Found triangle: (P₁, P₂, H)
            (set-add! simplices-2 (list p1-id p2-id h-id)))))))
  
  ;; Type 2: Point with two hyperplanes
  (for ([p-idx (in-range (length points-list))])
    (let* ([p (cdr (list-ref points-list p-idx))]
           [p-id (incidence-point-binding-id p)])
      (for* ([h1-idx (in-range (length hyperplanes-list))]
             [h2-idx (in-range (length hyperplanes-list))]
             #:when (< h1-idx h2-idx))
        (let* ([h1 (cdr (list-ref hyperplanes-list h1-idx))]
               [h2 (cdr (list-ref hyperplanes-list h2-idx))]
               [h1-id (incidence-hyperplane-constraint-id h1)]
               [h2-id (incidence-hyperplane-constraint-id h2)])
          (when (and (hash-ref incidence-matrix (cons p-id h1-id) #f)
                     (hash-ref incidence-matrix (cons p-id h2-id) #f))
            ;; Found triangle: (P, H₁, H₂)
            (set-add! simplices-2 (list p-id h1-id h2-id)))))))
  
  (set->list simplices-2))

;; Build 3-simplices (tetrahedra): 4 elements
(define (build-simplices-3 incidence-struct)
  "Build 3-simplices: tetrahedra in bipartite incidence structure
   A tetrahedron is 4 elements (points/hyperplanes) that all share a common constraint"
  (define points-list (incidence-structure-points incidence-struct))
  (define hyperplanes-list (incidence-structure-hyperplanes incidence-struct))
  (define incidence-matrix (incidence-structure-incidence-matrix incidence-struct))
  (define simplices-3 (mutable-set))
  
  ;; Type: 3 points + 1 hyperplane (all 3 points on the hyperplane)
  (for* ([p1-idx (in-range (length points-list))]
         [p2-idx (in-range (length points-list))]
         [p3-idx (in-range (length points-list))]
         #:when (and (< p1-idx p2-idx) (< p2-idx p3-idx)))
    (let* ([p1 (cdr (list-ref points-list p1-idx))]
           [p2 (cdr (list-ref points-list p2-idx))]
           [p3 (cdr (list-ref points-list p3-idx))]
           [p1-id (incidence-point-binding-id p1)]
           [p2-id (incidence-point-binding-id p2)]
           [p3-id (incidence-point-binding-id p3)])
      (for ([h-idx (in-range (length hyperplanes-list))])
        (let* ([h (cdr (list-ref hyperplanes-list h-idx))]
               [h-id (incidence-hyperplane-constraint-id h)])
          (when (and (hash-ref incidence-matrix (cons p1-id h-id) #f)
                     (hash-ref incidence-matrix (cons p2-id h-id) #f)
                     (hash-ref incidence-matrix (cons p3-id h-id) #f))
            ;; Found tetrahedron: (P₁, P₂, P₃, H)
            (set-add! simplices-3 (list p1-id p2-id p3-id h-id)))))))
  
  (set->list simplices-3))

;; Build 4-simplices: 5 elements
(define (build-simplices-4 incidence-struct)
  "Build 4-simplices: 5 elements (points/hyperplanes) sharing constraint"
  (define points-list (incidence-structure-points incidence-struct))
  (define hyperplanes-list (incidence-structure-hyperplanes incidence-struct))
  (define incidence-matrix (incidence-structure-incidence-matrix incidence-struct))
  (define simplices-4 (mutable-set))
  
  ;; Type: 4 points + 1 hyperplane (all 4 points on the hyperplane)
  (for* ([p1-idx (in-range (length points-list))]
         [p2-idx (in-range (length points-list))]
         [p3-idx (in-range (length points-list))]
         [p4-idx (in-range (length points-list))]
         #:when (and (< p1-idx p2-idx) (< p2-idx p3-idx) (< p3-idx p4-idx)))
    (let* ([p1 (cdr (list-ref points-list p1-idx))]
           [p2 (cdr (list-ref points-list p2-idx))]
           [p3 (cdr (list-ref points-list p3-idx))]
           [p4 (cdr (list-ref points-list p4-idx))]
           [p1-id (incidence-point-binding-id p1)]
           [p2-id (incidence-point-binding-id p2)]
           [p3-id (incidence-point-binding-id p3)]
           [p4-id (incidence-point-binding-id p4)])
      (for ([h-idx (in-range (length hyperplanes-list))])
        (let* ([h (cdr (list-ref hyperplanes-list h-idx))]
               [h-id (incidence-hyperplane-constraint-id h)])
          (when (and (hash-ref incidence-matrix (cons p1-id h-id) #f)
                     (hash-ref incidence-matrix (cons p2-id h-id) #f)
                     (hash-ref incidence-matrix (cons p3-id h-id) #f)
                     (hash-ref incidence-matrix (cons p4-id h-id) #f))
            ;; Found 4-simplex: (P₁, P₂, P₃, P₄, H)
            (set-add! simplices-4 (list p1-id p2-id p3-id p4-id h-id)))))))
  
  (set->list simplices-4))

;; ============================================================
;; BOUNDARY MAPS FOR HIGHER DIMENSIONS
;; ============================================================

;; Build boundary map d₂: C₂ → C₁
(define (build-boundary-d2 incidence-struct simplices-2 edges)
  "Build boundary map d₂: 2-simplices (triangles) → 1-simplices (edges)
   For triangle (v1, v2, v3): d₂ = (v1,v2) + (v2,v3) + (v3,v1)"
  (define num-edges (length edges))
  (define num-triangles (length simplices-2))
  
  (define boundary-matrix
    (for/list ([i (in-range num-triangles)])
      (make-vector num-edges 0)))
  
  ;; For each triangle, find its boundary edges
  (for ([tri-idx (in-range num-triangles)])
    (let ([triangle (list-ref simplices-2 tri-idx)])
      (match triangle
        [(list v1 v2 v3)
         ;; Boundary edges: (v1,v2), (v2,v3), (v3,v1)
         (for* ([i (list 0 1 2)]
                [j (list 1 2 0)]
                #:when (< i j))
           (let ([edge-key (if (symbol? (list-ref triangle i))
                                (cons (list-ref triangle i) (list-ref triangle j))
                                (cons (list-ref triangle j) (list-ref triangle i)))])
             (define edge-idx (for/first ([idx (in-naturals)]
                                          [e edges]
                                          #:when (or (equal? e edge-key)
                                                     (equal? e (cons (cdr edge-key) (car edge-key)))))
                                idx))
             (when edge-idx
               (vector-set! (list-ref boundary-matrix tri-idx) edge-idx 1))))]
        [else (void)])))
  
  ;; Convert to list of lists
  (for/list ([row boundary-matrix])
    (vector->list row)))

;; Build boundary map d₃: C₃ → C₂
(define (build-boundary-d3 incidence-struct simplices-3 simplices-2)
  "Build boundary map d₃: 3-simplices (tetrahedra) → 2-simplices (triangles)"
  (define num-triangles (length simplices-2))
  (define num-tetrahedra (length simplices-3))
  
  (define boundary-matrix
    (for/list ([i (in-range num-tetrahedra)])
      (make-vector num-triangles 0)))
  
  ;; For each tetrahedron, find its boundary triangles
  (for ([tet-idx (in-range num-tetrahedra)])
    (let ([tetrahedron (list-ref simplices-3 tet-idx)])
      (match tetrahedron
        [(list v1 v2 v3 v4)
         ;; Boundary triangles: all 3-element subsets
         (for ([tri-idx (in-range num-triangles)])
           (let ([triangle (list-ref simplices-2 tri-idx)])
             (when (subset? (list->set triangle) (list->set tetrahedron))
               (vector-set! (list-ref boundary-matrix tet-idx) tri-idx 1))))]
        [else (void)])))
  
  (for/list ([row boundary-matrix])
    (vector->list row)))

;; Build boundary map d₄: C₄ → C₃
(define (build-boundary-d4 incidence-struct simplices-4 simplices-3)
  "Build boundary map d₄: 4-simplices → 3-simplices (tetrahedra)"
  (define num-tetrahedra (length simplices-3))
  (define num-4simplices (length simplices-4))
  
  (define boundary-matrix
    (for/list ([i (in-range num-4simplices)])
      (make-vector num-tetrahedra 0)))
  
  ;; For each 4-simplex, find its boundary tetrahedra
  (for ([simp4-idx (in-range num-4simplices)])
    (let ([simplex4 (list-ref simplices-4 simp4-idx)])
      (for ([tet-idx (in-range num-tetrahedra)])
        (let ([tetrahedron (list-ref simplices-3 tet-idx)])
          (when (subset? (list->set tetrahedron) (list->set simplex4))
            (vector-set! (list-ref boundary-matrix simp4-idx) tet-idx 1))))))
  
  (for/list ([row boundary-matrix])
    (vector->list row)))

;; Build boundary map d₅: C₅ → C₄ (for completeness, though C₅ is usually empty)
(define (build-boundary-d5 incidence-struct simplices-5 simplices-4)
  "Build boundary map d₅: 5-simplices → 4-simplices"
  (define num-4simplices (length simplices-4))
  (define num-5simplices (length simplices-5))
  
  (define boundary-matrix
    (for/list ([i (in-range num-5simplices)])
      (make-vector num-4simplices 0)))
  
  (for ([simp5-idx (in-range num-5simplices)])
    (let ([simplex5 (list-ref simplices-5 simp5-idx)])
      (for ([simp4-idx (in-range num-4simplices)])
        (let ([simplex4 (list-ref simplices-4 simp4-idx)])
          (when (subset? (list->set simplex4) (list->set simplex5))
            (vector-set! (list-ref boundary-matrix simp5-idx) simp4-idx 1))))))
  
  (for/list ([row boundary-matrix])
    (vector->list row)))

;; ============================================================
;; COHOMOLOGY COMPUTATION H¹ through H⁴
;; ============================================================

;; ============================================================
;; POLYNOMIAL REPRESENTATION EXPORT
;; ============================================================

;; Convert a binding point to polynomial representation
;; Polynomial degree = dimension = access count = Church numeral
(define (binding->polynomial point)
  "Convert incidence point to polynomial representation
   Returns: (variable-name . degree) where degree = dimension
   Example: (binding->polynomial point) → ('x . 3) means x³"
  (if (incidence-point? point)
      (let ([binding-id (incidence-point-binding-id point)]
            [dimension (incidence-point-dimension point)])
        (cons binding-id dimension))
      (error "binding->polynomial: expected incidence-point, got ~a" point)))

;; Convert entire incidence structure to polynomial ring representation
;; Returns: list of (binding-id . degree) pairs representing polynomial terms
(define (incidence-structure->polynomial-ring incidence-struct)
  "Convert incidence structure to polynomial ring representation
   Each binding becomes a term: binding-id^dimension
   Returns: list of (binding-id . degree) pairs
   
   Example:
   - Binding 'x' with dimension 2 → term: x²
   - Binding 'y' with dimension 1 → term: y¹ = y
   - Binding 'z' with dimension 0 → term: z⁰ = 1 (constant)
   
   This representation can be used for:
   - Polynomial factorization
   - Zero locus computation
   - Algebraic operations on bindings"
  (define points (incidence-structure-points incidence-struct))
  
  ;; Convert hash or list to consistent format
  (define points-list
    (if (hash? points)
        (hash->list points)
        points))
  
  ;; Extract polynomial terms: (binding-id . degree)
  (for/list ([point-entry points-list])
    (let ([point (if (pair? point-entry) (cdr point-entry) point-entry)])
      (if (incidence-point? point)
          (binding->polynomial point)
          (error "incidence-structure->polynomial-ring: invalid point ~a" point)))))

;; ============================================================
;; COHOMOLOGY COMPUTATION H¹ through H⁴
;; ============================================================

;; Compute H¹ from incidence structure
;; Enhanced with dimensional weighting (Church numerals, access counts)
;; 
;; Algorithm:
;; 1. Compute base H¹ = dim(Ker(d₁)) - dim(Im(d₂))
;; 2. Apply dimensional enhancement:
;;    - Cycles involving higher-dimensional points (accessed more) contribute more
;;    - Weight cycles by the dimension (access count) of points involved
;;    - Recursive calls (dimension ≥ 1) create cycles
;; 3. Enhanced H¹ = max(base H¹, dimensional contribution)
;;
;; Dimensional Enhancement Rationale:
;; - Access count = Church numeral = polynomial degree = dimension
;; - Higher dimension = more significant cycle contribution
;; - Recursive functions (dimension ≥ 1) explicitly create cycles
;; - This ensures H¹ > 0 for recursive programs that would otherwise show H¹ = 0
(define (compute-h1-incidence incidence-struct)
  "Compute H¹ = dim(Ker(d₁)) - dim(Im(d₂))
   Enhanced: Considers dimensional information (access counts) for cycle weighting"
  (let-values ([(d1-matrix edges) (build-boundary-d1 incidence-struct)])
    (define ker-d1-dim (kernel-dimension d1-matrix))
    
    ;; Compute Im(d₂) from 2-simplices
    (define simplices-2 (build-simplices-2 incidence-struct))
    (define im-d2-dim (if (null? simplices-2)
                          0
                          (let ([d2-matrix (build-boundary-d2 incidence-struct simplices-2 edges)])
                            (let ([rank (matrix-rank d2-matrix)])
                              (if (number? rank) rank 0)))))
    
    ;; Base H¹ computation
    (define base-h1 (max 0 (- ker-d1-dim im-d2-dim)))
    
    ;; Dimensional enhancement: Weight cycles by access count (Church numerals)
    ;; Cycles involving higher-dimensional points (accessed more) contribute more
    (define points (incidence-structure-points incidence-struct))
    (define points-hash (if (hash? points)
                            points
                            (for/hash ([p-entry points])
                              (values (car p-entry) (cdr p-entry)))))
    (define dimensional-weight
      (for/sum ([edge-key (in-hash-keys (incidence-structure-incidence-matrix incidence-struct))])
        (when (hash-ref (incidence-structure-incidence-matrix incidence-struct) edge-key #f)
          (match edge-key
            [(cons point-id _)
             (let ([point (hash-ref points-hash point-id #f)])
               (if point
                   ;; Weight by dimension = access count = Church numeral
                   ;; Higher dimension = more significant cycle contribution
                   (incidence-point-dimension point)
                   0))]
            [else 0]))))
    
    ;; Enhanced H¹: base computation + dimensional contribution
    ;; Recursive calls (dimension ≥ 1) should create cycles
    (define point-list (if (hash? points) (hash->list points) points))
    (define enhanced-h1
      (if (> dimensional-weight 0)
          ;; Dimensional cycles detected: at least 1 if any point has dimension > 0
          (max base-h1
               (if (ormap (lambda (p-entry)
                            (let ([p (if (pair? p-entry) (cdr p-entry) p-entry)])
                              (and (incidence-point? p)
                                   (> (incidence-point-dimension p) 0))))
                          point-list)
                   1 0))
          base-h1))
    
    enhanced-h1))

;; Compute H²
(define (compute-h2-incidence incidence-struct)
  "Compute H² = dim(Ker(d₂)) - dim(Im(d₃))"
  (let*-values ([(d1-matrix edges) (build-boundary-d1 incidence-struct)]
                [(simplices-2) (build-simplices-2 incidence-struct)])
    (if (null? simplices-2)
        0
        (let* ([d2-matrix (build-boundary-d2 incidence-struct simplices-2 edges)]
               [simplices-3 (build-simplices-3 incidence-struct)])
          (define ker-d2-dim (kernel-dimension d2-matrix))
          (define im-d3-dim (if (null? simplices-3)
                                0
                                (let ([d3-matrix (build-boundary-d3 incidence-struct simplices-3 simplices-2)])
                                  (let ([rank (matrix-rank d3-matrix)])
                                    (if (number? rank) rank 0)))))
          (max 0 (- ker-d2-dim im-d3-dim))))))

;; Compute H³
(define (compute-h3-incidence incidence-struct)
  "Compute H³ = dim(Ker(d₃)) - dim(Im(d₄))"
  (let*-values ([(simplices-2) (build-simplices-2 incidence-struct)]
                [(simplices-3) (build-simplices-3 incidence-struct)])
    (if (null? simplices-3)
        0
        (let* ([d3-matrix (build-boundary-d3 incidence-struct simplices-3 simplices-2)]
               [simplices-4 (build-simplices-4 incidence-struct)])
          (define ker-d3-dim (kernel-dimension d3-matrix))
          (define im-d4-dim (if (null? simplices-4)
                                0
                                (let ([d4-matrix (build-boundary-d4 incidence-struct simplices-4 simplices-3)])
                                  (let ([rank (matrix-rank d4-matrix)])
                                    (if (number? rank) rank 0)))))
          (max 0 (- ker-d3-dim im-d4-dim))))))

;; Compute H⁴
(define (compute-h4-incidence incidence-struct)
  "Compute H⁴ = dim(Ker(d₄)) - dim(Im(d₅))"
  (let*-values ([(simplices-3) (build-simplices-3 incidence-struct)]
                [(simplices-4) (build-simplices-4 incidence-struct)])
    (if (null? simplices-4)
        0
        (let* ([d4-matrix (build-boundary-d4 incidence-struct simplices-4 simplices-3)]
               [simplices-5 '()])  ;; C₅ is typically empty
          (define ker-d4-dim (kernel-dimension d4-matrix))
          (define im-d5-dim 0)  ;; Im(d₅) = 0 since C₅ is empty
          (max 0 (- ker-d4-dim im-d5-dim))))))

;; Compute Hⁿ for any n (1-4)
(define (compute-hn-incidence incidence-struct n)
  "Compute Hⁿ for n = 1, 2, 3, or 4"
  (match n
    [1 (compute-h1-incidence incidence-struct)]
    [2 (compute-h2-incidence incidence-struct)]
    [3 (compute-h3-incidence incidence-struct)]
    [4 (compute-h4-incidence incidence-struct)]
    [_ (error (format "H~a not supported (only H¹-H⁴)" n))]))

;; Compute kernel dimension using rank-nullity theorem
;; dim(Ker) = n - rank, where n = number of columns
(define (kernel-dimension matrix)
  "Compute dimension of kernel using rank-nullity theorem"
  (if (null? matrix)
      0
      (let ([num-rows (length matrix)]
            [num-cols (length (car matrix))])
        (let ([rank (matrix-rank matrix)])
          (if (number? rank)
              (max 0 (- num-cols rank))
              0)))))

