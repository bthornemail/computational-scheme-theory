#lang racket

;; Metrics API - HTTP/JSON Service (MVP)
;; Provides HTTP API for computing V(G) cyclomatic complexity

(require net/http-server
         json
         "r5rs-parser.rkt"
         "cfg-builder.rkt"
         "cyclomatic.rkt")

(provide start-metrics-service)

;; Handler for computing V(G)
(define (compute-vg-handler request)
  (let* ([body-json (read-json (request-post-data/raw request))]
         [program-id (hash-ref body-json 'program_id (λ () ""))]
         [source-code (hash-ref body-json 'source_code (λ () ""))])
    
    (with-handlers ([exn:fail? (λ (e)
                                  (response/json
                                   #hash(("success" . #f)
                                         ("program_id" . ,program-id)
                                         ("error" . ,(exn-message e)))))]
                    [exn? (λ (e)
                            (response/json
                             #hash(("success" . #f)
                                   ("program_id" . ,program-id)
                                   ("error" . ,(format "~a" e)))))])
      ;; Parse → CFG → V(G)
      (let* ([ast-list (parse-r5rs source-code)]
             [cfg (build-cfg (first ast-list))]
             [metrics (compute-cyclomatic-complexity cfg)])
        
        ;; Return result
        (response/json
         #hash(("success" . #t)
               ("program_id" . ,program-id)
               ("v_g" . ,(complexity-metrics-v-g metrics))
               ("nodes" . ,(complexity-metrics-nodes metrics))
               ("edges" . ,(complexity-metrics-edges metrics))
               ("components" . ,(complexity-metrics-components metrics))
               ("formula" . ,(complexity-metrics-formula metrics))
               ("error" . "")))))))

;; Health check handler
(define (health-check-handler request)
  (response/json
   #hash(("healthy" . #t)
         ("service" . "metrics-calculator")
         ("version" . "0.1.0"))))

;; Request router
(define (route-handler request)
  (match (request-uri-path request)
    ["/compute-vg" #:when (eq? (request-method request) 'POST)
     (compute-vg-handler request)]
    ["/health" #:when (eq? (request-method request) 'GET)
     (health-check-handler request)]
    [_ (response/json
        #hash(("error" . "Not found"))
        #:code 404)]))

;; Start HTTP server
(define (start-metrics-service [port 50052])
  (printf "Starting metrics service on port ~a\n" port)
  (serve/servlet route-handler
                #:port port
                #:listen-ip #f
                #:launch-browser? #f
                #:servlet-path "/"
                #:servlet-regexp #rx""))

