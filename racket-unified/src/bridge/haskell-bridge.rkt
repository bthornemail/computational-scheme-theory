#lang racket/base

(require net/http-client
         net/url
         json
         racket/match
         racket/string)

(provide
 call-haskell-h1
 compare-h1-results
 haskell-service-available?)

;; ============================================================
;; HASKELL SERVICE BRIDGE
;; ============================================================

;; Configuration
(define *haskell-service-url* (make-parameter "http://localhost:8080/api/compute-h1"))

;; Check if Haskell service is available
(define (haskell-service-available?)
  "Check if Haskell service is reachable"
  (with-handlers ([exn? (lambda (e) #f)])
    (let* ([url (*haskell-service-url*)]
           [uri (string->url url)]
           [host (url-host uri)]
           [port (or (url-port uri) 80)]
           [conn (http-conn-open host #:port port)])
      (begin0
        (let-values ([(status headers-port) (http-conn-sendrecv! conn "/api/health" #:method "GET")])
          ;; status is a number (HTTP status code)
          (let ([status-code (if (number? status) status (string->number (car (string-split (if (string? status) status "200")))))])
            (and status-code (= status-code 200))))
        (http-conn-close! conn)))))

;; Call Haskell service to compute H¹
(define (call-haskell-h1 source)
  "Call existing Haskell service to compute H¹ from source"
  (with-handlers ([exn? (lambda (e) (values #f (exn-message e)))])
    (let* ([url (*haskell-service-url*)]
           [uri (string->url url)]
           [host (url-host uri)]
           [path-list (url-path uri)]
           [path (if (null? path-list)
                     "/api/compute-h1"
                     (string-append "/" (string-join (map symbol->string path-list) "/")))]
           [port (or (url-port uri) 80)]
           [post-data (jsexpr->string (hash 'source source))]
           [headers (list "Content-Type: application/json")]
           [conn (http-conn-open host #:port port)])
      (let-values ([(status headers-port) (http-conn-sendrecv!
                                          conn
                                          path
                                          #:method "POST"
                                          #:data post-data
                                          #:headers headers)])
        (let ([status-code (if (number? status) status (string->number (car (string-split (if (string? status) status "500")))))]
              [body-json (read-string 10240 headers-port)])  ; Read up to 10KB
          (http-conn-close! conn)
          (if (and status-code (= status-code 200))
              (let ([data (string->jsexpr body-json)])
                (values (hash-ref data 'h1 #f) #f))
              (values #f (format "HTTP ~a: ~a" status-code body-json))))))))

;; Compare Lisp and Haskell results
(define (compare-h1-results lisp-result haskell-result tolerance)
  "Compare H¹ results from Lisp and Haskell implementations"
  (cond
    [(and lisp-result haskell-result)
     (let ([diff (abs (- lisp-result haskell-result))])
       (if (<= diff tolerance)
           (values #t diff "Results match within tolerance")
           (values #f diff (format "Results differ by ~a (tolerance: ~a)" diff tolerance))))]
    [(not lisp-result)
     (values #f #f "Lisp computation failed")]
    [(not haskell-result)
     (values #f #f "Haskell service unavailable or failed")]
    [else
     (values #f #f "Both computations failed")]))


