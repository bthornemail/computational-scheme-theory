#lang racket/base

(require racket/match
         racket/set
         racket/port)

(provide
 parse-r5rs
 alpha-convert
 extract-binding-algebra
 binding?
 r-scheme?
 r-scheme-bindings
 ;; AST structs and predicates
 ast-const
 ast-const?
 ast-var
 ast-var?
 ast-lambda
 ast-lambda?
 ast-lambda-params
 ast-lambda-body
 ast-let
 ast-let?
 ast-let-bindings
 ast-let-body
 ast-letrec
 ast-letrec?
 ast-letrec-bindings
 ast-letrec-body
 ast-define
 ast-define?
 ast-define-name
 ast-define-value
 ast-if
 ast-if?
 ast-if-test
 ast-if-then
 ast-if-else
 ast-app
 ast-app?
 ast-app-func
 ast-app-args
 sexpr->ast
 extract-bindings)

;; ============================================================
;; ALGORITHM 1: BINDING ALGEBRA EXTRACTION
;; ============================================================

;; R_Scheme rig: Set of bindings
(struct r-scheme (bindings) #:transparent)

;; Binding identifier
(define binding? symbol?)

;; Source location (simplified for now)
(struct source-loc (file line col) #:transparent)

;; AST representation
(struct ast-const (loc value) #:transparent)
(struct ast-var (loc name) #:transparent)
(struct ast-lambda (loc params body) #:transparent)
(struct ast-let (loc bindings body) #:transparent)
(struct ast-letrec (loc bindings body) #:transparent)
(struct ast-define (loc name value) #:transparent)
(struct ast-if (loc test then else) #:transparent)
(struct ast-app (loc func args) #:transparent)

;; Parse R5RS Scheme source code
;; Uses Racket's built-in read function to parse S-expressions
(define (parse-r5rs source)
  "Parse R5RS Scheme source to AST"
  (cond
    [(string? source) (parse-r5rs-string source)]
    [(input-port? source) (parse-r5rs-port source)]
    [else (error "Invalid source type: expected string or port")]))

(define (parse-r5rs-string str)
  "Parse from string"
  (with-input-from-string str
    (lambda () (parse-r5rs-port (current-input-port)))))

(define (parse-r5rs-port port)
  "Parse from port, returning list of AST nodes"
  (let loop ([exprs '()])
    (let ([expr (read port)])
      (if (eof-object? expr)
          (reverse (map sexpr->ast exprs))
          (loop (cons expr exprs))))))

;; Convert S-expression to AST
(define (sexpr->ast sexpr)
  "Convert S-expression to AST node"
  (sexpr->ast-with-loc sexpr (source-loc "" 1 1)))

(define (sexpr->ast-with-loc sexpr loc)
  "Convert S-expression to AST with source location"
  (match sexpr
    ;; Constants
    [(? number? n) (ast-const loc n)]
    [(? string? s) (ast-const loc s)]
    [(? boolean? b) (ast-const loc b)]
    [(? char? c) (ast-const loc c)]
    ['() (ast-const loc '())]
    
    ;; Variables
    [(? symbol? sym) (ast-var loc sym)]
    
    ;; Lambda
    [`(lambda ,params ,body ...)
     (ast-lambda loc 
                 (if (list? params) params (list params))
                 (map sexpr->ast body))]
    
    ;; Define variable
    [`(define ,name ,value)
     (ast-define loc name (sexpr->ast value))]
    
    ;; Define function: (define (name params...) body...)
    [`(define (,name ,params ...) ,body ...)
     (ast-define loc name (ast-lambda loc params (map sexpr->ast body)))]
    
    ;; Let
    [`(let ,bindings ,body ...)
     (ast-let loc 
              (map (lambda (b)
                     (match b
                       [`(,name ,value) (cons name (sexpr->ast value))]))
                   bindings)
              (map sexpr->ast body))]
    
    ;; Letrec
    [`(letrec ,bindings ,body ...)
     (ast-letrec loc
                 (map (lambda (b)
                        (match b
                          [`(,name ,value) (cons name (sexpr->ast value))]))
                      bindings)
                 (map sexpr->ast body))]
    
    ;; If
    [`(if ,test ,then)
     (ast-if loc (sexpr->ast test) (sexpr->ast then) (ast-const loc #f))]
    [`(if ,test ,then ,else)
     (ast-if loc (sexpr->ast test) (sexpr->ast then) (sexpr->ast else))]
    
    ;; Application
    [`(,func . ,args)
     (ast-app loc (sexpr->ast func) (map sexpr->ast args))]
    
    [else (error "Unsupported S-expression:" sexpr)]))

;; Alpha conversion (hygienic renaming)
;; Simple implementation: generate unique binding names
(define alpha-counter-box (box 0))

(define (get-next-alpha)
  (let ([count (unbox alpha-counter-box)])
    (set-box! alpha-counter-box (+ count 1))
    (string->symbol (format "α~a" count))))

(define (reset-alpha-counter)
  (set-box! alpha-counter-box 0))

(define (alpha-convert expr)
  "Apply alpha conversion to expression, returning new AST with hygienic names"
  (reset-alpha-counter)
  (alpha-convert-expr expr (make-hash)))

(define (alpha-convert-expr expr env)
  "Convert expression with environment mapping old names to new ones"
  (match expr
    [(ast-const loc value) expr]
    
    [(ast-var loc name)
     (if (hash-has-key? env name)
         (ast-var loc (hash-ref env name))
         expr)]
    
    [(ast-lambda loc params body)
     (let* ([new-env (hash-copy env)]
            [new-params (map (lambda (p)
                                (let ([new-name (get-next-alpha)])
                                  (hash-set! new-env p new-name)
                                  new-name))
                              params)]
            [new-body (map (lambda (b) (alpha-convert-expr b new-env)) body)])
       (ast-lambda loc new-params new-body))]
    
    [(ast-let loc bindings body)
     (let* ([new-env (hash-copy env)]
            [new-bindings (map (lambda (b)
                                  (let ([name (car b)]
                                        [value (cdr b)]
                                        [new-name (get-next-alpha)])
                                    (hash-set! new-env name new-name)
                                    (cons new-name (alpha-convert-expr value new-env))))
                                bindings)]
            [new-body (map (lambda (b) (alpha-convert-expr b new-env)) body)])
       (ast-let loc new-bindings new-body))]
    
    [(ast-letrec loc bindings body)
     ;; Similar to let but bindings visible in each other
     (let* ([new-env (hash-copy env)]
            [new-bindings (map (lambda (b)
                                  (let ([name (car b)]
                                        [new-name (get-next-alpha)])
                                    (hash-set! new-env name new-name)
                                    (cons new-name '())))
                                bindings)]
            [final-bindings (map (lambda (b new-b)
                                    (let ([value (cdr b)]
                                          [new-name (car new-b)])
                                      (cons new-name (alpha-convert-expr value new-env))))
                                  bindings new-bindings)]
            [new-body (map (lambda (b) (alpha-convert-expr b new-env)) body)])
       (ast-letrec loc final-bindings new-body))]
    
    [(ast-define loc name value)
     (let ([new-name (get-next-alpha)])
       (let ([new-env (hash-copy env)])
         (hash-set! new-env name new-name)
         (ast-define loc new-name (alpha-convert-expr value new-env))))]
    
    [(ast-if loc test then else)
     (ast-if loc
             (alpha-convert-expr test env)
             (alpha-convert-expr then env)
             (alpha-convert-expr else env))]
    
    [(ast-app loc func args)
     (ast-app loc
              (alpha-convert-expr func env)
              (map (lambda (a) (alpha-convert-expr a env)) args))]
    
    [else expr]))

;; Extract binding algebra from Scheme source
(define (extract-binding-algebra source)
  "Extract R_Scheme rig from Scheme source code"
  (let* ([exprs (parse-r5rs source)]
         [converted-exprs (map alpha-convert exprs)]
         [bindings (foldr set-union (set)
                          (map extract-bindings converted-exprs))])
    (r-scheme bindings)))

;; Extract bindings from an AST node
(define (extract-bindings expr)
  "Extract all binding identifiers from expression"
  (match expr
    [(ast-const loc value) (set)]
    
    [(ast-var loc name) (set)]
    
    [(ast-lambda loc params body)
     (set-union (list->set params)
                (foldr set-union (set) (map extract-bindings body)))]
    
    [(ast-let loc bindings body)
     (set-union (list->set (map car bindings))
                (foldr set-union (set) (map extract-bindings (map cdr bindings)))
                (foldr set-union (set) (map extract-bindings body)))]
    
    [(ast-letrec loc bindings body)
     (set-union (list->set (map car bindings))
                (foldr set-union (set) (map extract-bindings (map cdr bindings)))
                (foldr set-union (set) (map extract-bindings body)))]
    
    [(ast-define loc name value)
     (set-add (extract-bindings value) name)]
    
    [(ast-if loc test then else)
     (set-union (extract-bindings test)
                (extract-bindings then)
                (extract-bindings else))]
    
    [(ast-app loc func args)
     (set-union (extract-bindings func)
                (foldr set-union (set) (map extract-bindings args)))]
    
    [else (set)]))

