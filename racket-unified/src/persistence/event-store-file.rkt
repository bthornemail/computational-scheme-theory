#lang racket/base

(require racket/file
         racket/path
         racket/port
         "config.rkt")

(provide
 load-events-from-file
 append-event-to-file
 replay-events-from-file
 ensure-event-log-directory)

;; ============================================================
;; FILE-BASED EVENT STORE
;; ============================================================

;; Ensure event log directory exists
(define (ensure-event-log-directory)
  "Ensure event log directory exists"
  (let* ([log-path (get-event-log-path)]
         [log-path-obj (if (string? log-path) (string->path log-path) log-path)]
         [dir (path-only log-path-obj)])
    (when dir
      (make-directory* dir))))

;; Load all events from file
(define (load-events-from-file)
  "Load all events from event log file"
  (let ([log-path (get-event-log-path)])
    (if (file-exists? log-path)
        (let ([events '()])
          (with-input-from-file log-path
            (lambda ()
              (let loop ([acc '()])
                (let ([event (read)])
                  (if (eof-object? event)
                      (reverse acc)
                      (loop (cons event acc))))))))
        '())))

;; Append event to file (atomic write)
(define (append-event-to-file event)
  "Append S-expression event to log file atomically"
  (ensure-event-log-directory)
  (let ([log-path (get-event-log-path)])
    ;; Use append mode for thread-safe writes
    (call-with-output-file log-path
      (lambda (out)
        (write event out)
        (newline out)
        (flush-output out))
      #:exists 'append
      #:mode 'text)))

;; Replay events from file (streaming)
(define (replay-events-from-file [filter-fn #f])
  "Replay events from file, optionally filtering"
  (let ([log-path (get-event-log-path)])
    (if (file-exists? log-path)
        (let ([events '()])
          (with-input-from-file log-path
            (lambda ()
              (let loop ([acc '()])
                (let ([event (read)])
                  (if (eof-object? event)
                      (reverse acc)
                      (if (or (not filter-fn) (filter-fn event))
                          (loop (cons event acc))
                          (loop acc))))))))
        '())))

