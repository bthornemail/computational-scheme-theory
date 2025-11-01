#lang racket/base

(require net/http-client
         net/url
         json
         racket/match
         racket/string)

(provide
 call-racket-vg
 validate-hypothesis
 racket-service-available?)

;; ============================================================
;; RACKET SERVICE BRIDGE
;; ============================================================

;; Configuration
(define *racket-service-url* (make-parameter "http://localhost:8081/api/compute-vg"))

;; Check if Racket service is available
(define (racket-service-available?)
  "Check if Racket metrics service is reachable"
  (with-handlers ([exn? (lambda (e) #f)])
    (let* ([url (*racket-service-url*)]
           [uri (string->url url)]
           [host (url-host uri)]
           [port (or (url-port uri) 80)]
           [conn (http-conn-open host #:port port)])
      (begin0
        (let-values ([(status headers-port) (http-conn-sendrecv! conn "/api/health" #:method "GET")])
          (let ([status-code (if (number? status) status (string->number (car (string-split (if (string? status) status "200")))))])
            (and status-code (= status-code 200))))
        (http-conn-close! conn)))))

;; Call Racket service to compute V(G)
(define (call-racket-vg source)
  "Call existing Racket service to compute V(G) from source"
  (with-handlers ([exn? (lambda (e) (values #f (exn-message e)))])
    (let* ([url (*racket-service-url*)]
           [uri (string->url url)]
           [host (url-host uri)]
           [path-list (url-path uri)]
           [path (if (null? path-list)
                     "/api/compute-vg"
                     (string-append "/" (string-join (map symbol->string path-list) "/")))]
           [port (or (url-port uri) 80)]
           [post-data (jsexpr->string (hash 'source_code source))]
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
                (if (hash-ref data 'success #f)
                    (values (hash-ref data 'v_g #f) #f)
                    (values #f (hash-ref data 'error "Unknown error"))))
              (values #f (format "HTTP ~a: ~a" status-code body-json))))))))

;; Validate hypothesis: H¹ = V(G) - k (where k is typically 0 or 1)
(define (validate-hypothesis h1 vg [k 0] [tolerance 0])
  "Validate that H¹ = V(G) - k within tolerance"
  (cond
    [(and h1 vg)
     (let* ([expected-h1 (- vg k)]
            [diff (abs (- h1 expected-h1))])
       (if (<= diff tolerance)
           (values #t diff (format "Hypothesis validated: H¹=~a = V(G)-k=~a (diff: ~a)" h1 expected-h1 diff))
           (values #f diff (format "Hypothesis violated: H¹=~a ≠ V(G)-k=~a (diff: ~a)" h1 expected-h1 diff))))]
    [(not h1)
     (values #f #f "H¹ computation failed")]
    [(not vg)
     (values #f #f "V(G) computation failed")]
    [else
     (values #f #f "Both computations failed")]))


