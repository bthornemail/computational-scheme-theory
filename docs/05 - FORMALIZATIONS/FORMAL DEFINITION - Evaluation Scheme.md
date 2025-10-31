;; ============================================================
;; FORMAL DEFINITION: Evaluation Scheme
;; Mapping R5RS eval/apply to Grothendieck's Spec functor
;; ============================================================

;; ------------------------------------------------------------
;; 1. ENVIRONMENTS AS RINGS
;; ------------------------------------------------------------

;; An environment is a finite map Var → Value
;; We make it "ring-like" by defining operations:

(define (env-empty)
  '())

(define (env-extend var val env)
  (cons (cons var val) env))

(define (env-lookup var env)
  (cond
    ((null? env) 
     (error "Unbound variable" var))
    ((eq? var (caar env)) 
     (cdar env))
    (else 
     (env-lookup var (cdr env)))))

;; "Ring operations" on environments:
;; - Addition: merge environments (like R₁ ⊗ R₂)
;; - Multiplication: compose environments (like R₁ ×_S R₂)

(define (env-union env1 env2)
  "Merge two environments (later bindings shadow earlier)"
  (append env2 env1))

(define (env-compose env1 env2)
  "Compose environments: eval in env1, then env2"
  (map (lambda (binding)
         (cons (car binding)
               (if (procedure? (cdr binding))
                   (cdr binding)
                   (cdr binding))))
       (append env1 env2)))

;; ------------------------------------------------------------
;; 2. EXPRESSIONS AS SPECTRA
;; ------------------------------------------------------------

;; An expression e with free vars {x₁, ..., xₙ} defines
;; a "spectrum" Spec(e) = {ρ : eval(e, ρ) is defined}

(define (free-vars expr)
  "Return list of free variables in expression"
  (cond
    ((symbol? expr) (list expr))
    ((not (pair? expr)) '())
    ((eq? (car expr) 'lambda)
     (let ((bound-vars (cadr expr))
           (body-vars (free-vars (caddr expr))))
       (filter (lambda (v) (not (memq v bound-vars))) 
               body-vars)))
    ((eq? (car expr) 'quote) '())
    (else
     (apply append (map free-vars expr)))))

(define (spec expr)
  "Spectrum of expr = function from envs to values"
  (lambda (env)
    (eval-expr expr env)))

;; ------------------------------------------------------------
;; 3. EVALUATION AS SPEC FUNCTOR
;; ------------------------------------------------------------

(define (eval-expr expr env)
  "Core evaluator (simplified R5RS subset)"
  (cond
    ;; Self-evaluating
    ((number? expr) expr)
    ((boolean? expr) expr)
    ((string? expr) expr)
    
    ;; Variable lookup
    ((symbol? expr) 
     (env-lookup expr env))
    
    ;; Quote
    ((and (pair? expr) (eq? (car expr) 'quote))
     (cadr expr))
    
    ;; Lambda (closure captures environment)
    ((and (pair? expr) (eq? (car expr) 'lambda))
     (let ((params (cadr expr))
           (body (caddr expr)))
       (lambda args
         (let ((new-env (env-union 
                         (map cons params args)
                         env)))
           (eval-expr body new-env)))))
    
    ;; If
    ((and (pair? expr) (eq? (car expr) 'if))
     (if (eval-expr (cadr expr) env)
         (eval-expr (caddr expr) env)
         (eval-expr (cadddr expr) env)))
    
    ;; Application
    ((pair? expr)
     (let ((proc (eval-expr (car expr) env))
           (args (map (lambda (e) (eval-expr e env)) 
                     (cdr expr))))
       (apply proc args)))
    
    (else (error "Cannot evaluate" expr))))

;; ------------------------------------------------------------
;; 4. MORPHISMS: SUBSTITUTION AS RING HOMOMORPHISM
;; ------------------------------------------------------------

(define (substitute expr var value)
  "Substitute value for var in expr (like φ: R → S)"
  (cond
    ((eq? expr var) value)
    ((symbol? expr) expr)
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'quote) expr)
    ((and (pair? expr) (eq? (car expr) 'lambda))
     (if (memq var (cadr expr))
         expr  ; var is shadowed, don't substitute
         (list 'lambda 
               (cadr expr)
               (substitute (caddr expr) var value))))
    (else
     (map (lambda (e) (substitute e var value)) expr))))

;; The induced scheme morphism φ*: Spec(e[x/t]) → Spec(e)
(define (spec-morphism expr var value-expr)
  "Contravariant: substitution induces backward map"
  (let ((substituted-expr (substitute expr var value-expr)))
    (lambda (env)
      ;; This is the BACKWARD map:
      ;; To eval substituted expr in env,
      ;; we eval original expr in extended env
      (eval-expr expr 
                 (env-extend var 
                            (eval-expr value-expr env)
                            env)))))

;; ------------------------------------------------------------
;; 5. LOCALIZATION: PARTIAL EVALUATION
;; ------------------------------------------------------------

(define (localize expr env)
  "Partial evaluation = localization at prime ideal (free vars)"
  (let ((fv (free-vars expr)))
    (if (null? fv)
        ;; No free vars → fully evaluated (global section)
        (eval-expr expr env)
        ;; Has free vars → partially evaluated (local ring)
        (list 'localized expr env fv))))

;; ------------------------------------------------------------
;; 6. COHOMOLOGY: CONTINUATION STRUCTURE
;; ------------------------------------------------------------

(define (capture-continuation proc)
  "Capture current continuation (like H⁰ cohomology)"
  (call/cc (lambda (k) (proc k))))

;; Delimited continuation = cohomology with support
(define (reset thunk)
  "Reset = take cohomology over region"
  (call/cc (lambda (k)
             (k (thunk)))))

(define (shift proc)
  "Shift = local-to-global section"
  (call/cc (lambda (k)
             (k (proc k)))))

;; ------------------------------------------------------------
;; 7. EXAMPLES: SEEING THE STRUCTURE
;; ------------------------------------------------------------

;; Example 1: Simple expression as spectrum
(define expr1 '(+ x (* y 2)))
(define spectrum1 (spec expr1))

(display "Example 1: Spectrum of (+ x (* y 2))\n")
(display "Free vars: ")
(display (free-vars expr1))
(newline)
(display "Eval at {x: 5, y: 3}: ")
(display (spectrum1 (list (cons 'x 5) (cons 'y 3))))
(newline)
(newline)

;; Example 2: Substitution as morphism
(define expr2 '(+ x y))
(define morphism (spec-morphism expr2 'y '(* z 2)))

(display "Example 2: Morphism via substitution y ↦ (* z 2)\n")
(display "Original: (+ x y)\n")
(display "Eval at {x: 5, y: 3}: ")
(display ((spec expr2) (list (cons 'x 5) (cons 'y 3))))
(newline)
(display "After morphism, eval at {x: 5, z: 10}: ")
(display (morphism (list (cons 'x 5) (cons 'z 10))))
(newline)
(newline)

;; Example 3: Closure as localized ring
(define closure-expr '(lambda (y) (+ x y)))
(define closure-val (eval-expr closure-expr 
                               (list (cons 'x 10))))

(display "Example 3: Closure as localization\n")
(display "Expression: (lambda (y) (+ x y))\n")
(display "Captured environment: {x: 10}\n")
(display "Applied to 5: ")
(display (closure-val 5))
(newline)
(newline)

;; Example 4: Continuation as cohomology
(display "Example 4: Continuation as cohomology\n")
(display "Captured continuation structure:\n")
(let ((result 
       (capture-continuation
        (lambda (k)
          (display "  - Continuation k captured\n")
          (+ 10 (k 5))))))  ; k exits early with 5
  (display "  - Result: ")
  (display result)
  (newline))

;; ============================================================
;; THEORETICAL SUMMARY
;; ============================================================

(display "\n")
(display "==================================================\n")
(display "CORRESPONDENCE SUMMARY\n")
(display "==================================================\n")
(display "Ring R              ↔ Environment ρ\n")
(display "Prime ideal P       ↔ Free variable x\n")
(display "Spec(R)             ↔ Evaluation function\n")
(display "Ring hom φ: R → S   ↔ Substitution σ: x ↦ t\n")
(display "Scheme map φ*       ↔ Backward evaluation\n")
(display "Localization R_p    ↔ Partial evaluation\n")
(display "Global sections Γ   ↔ Closed terms\n")
(display "Sheaf cohomology    ↔ Continuation structure\n")
(display "==================================================\n")