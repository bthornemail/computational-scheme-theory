#lang info
(define collection "racket-unified")
(define version "0.1.0")
(define deps
  '("base"
    "rackunit-lib"
    "math-lib"
    "net-lib"  ; For HTTP client
    "web-server-lib"))  ; For HTTP utilities
;; Note: minikanren will be added when we upgrade Prolog engine
(define build-deps '())

