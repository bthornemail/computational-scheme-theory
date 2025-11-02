#lang racket/base

(require racket/string
         racket/list)

(provide
 string-similarity
 fuzzy-match
 fuzzy-match-list
 levenshtein-distance)

;; ============================================================
;; FUZZY MATCHING - Pure Racket Typo Tolerance
;; ============================================================

;; Simple string similarity (0-1 scale)
(define (string-similarity s1 s2)
  "Compute similarity between two strings (0.0 to 1.0)"
  (let ([s1-lower (string-downcase s1)]
        [s2-lower (string-downcase s2)]
        [len1 (string-length s1)]
        [len2 (string-length s2)])
    (cond
      ;; Exact match
      [(string=? s1-lower s2-lower) 1.0]
      
      ;; Same length - character overlap
      [(= len1 len2)
       (let ([matches (for/sum ([c1 s1-lower] [c2 s2-lower] #:when (char=? c1 c2)) 1)])
         (/ matches len1))]
      
      ;; One is prefix of another
      [(string-prefix? s1-lower s2-lower) 
       (min 0.9 (/ len1 (max len2 1)))]
      [(string-prefix? s2-lower s1-lower) 
       (min 0.9 (/ len2 (max len1 1)))]
      
      ;; One contains another
      [(string-contains? s1-lower s2-lower)
       (min 0.7 (/ len2 (max len1 1)))]
      [(string-contains? s2-lower s1-lower)
       (min 0.7 (/ len1 (max len2 1)))]
      
      ;; Use Levenshtein for others
      [else
       (let ([max-len (max len1 len2)])
         (if (zero? max-len)
             1.0
             (- 1.0 (/ (levenshtein-distance s1-lower s2-lower) max-len))))])))

;; Levenshtein distance (edit distance)
(define (levenshtein-distance s1 s2)
  "Compute Levenshtein edit distance between two strings"
  (let ([len1 (string-length s1)]
        [len2 (string-length s2)])
    (cond
      [(zero? len1) len2]
      [(zero? len2) len1]
      [else
       ;; Simplified Levenshtein distance using dynamic programming
       (define matrix (make-vector (+ 1 len1)))
       ;; Initialize matrix
       (for ([i (in-range (+ 1 len1))])
         (define row (make-vector (+ 1 len2)))
         (vector-set! matrix i row)
         (vector-set! row 0 i))
       ;; Initialize first row
       (define first-row (vector-ref matrix 0))
       (for ([j (in-range 1 (+ 1 len2))])
         (vector-set! first-row j j))
       ;; Fill matrix
       (for ([i (in-range 1 (+ 1 len1))])
         (define row-i (vector-ref matrix i))
         (define row-i-1 (vector-ref matrix (- i 1)))
         (for ([j (in-range 1 (+ 1 len2))])
           (let ([cost (if (char=? (string-ref s1 (- i 1)) (string-ref s2 (- j 1))) 0 1)]
                 [sub (vector-ref row-i-1 (- j 1))]
                 [ins (vector-ref row-i (- j 1))]
                 [del (vector-ref row-i-1 j)])
             (vector-set! row-i j (min (+ sub cost) (+ ins 1) (+ del 1))))))
       ;; Return final value
       (vector-ref (vector-ref matrix len1) len2)])))

;; Fuzzy match word against single candidate
(define (fuzzy-match word candidate threshold)
  "Match word against candidate with similarity threshold"
  (let ([similarity (string-similarity word candidate)])
    (if (> similarity threshold)
        (values candidate similarity)
        (values #f 0.0))))

;; Fuzzy match word against list of candidates
(define (fuzzy-match-list word candidate-list threshold)
  "Find best matching word from list if similarity > threshold"
  (for/fold ([best-match #f]
             [best-score 0.0])
            ([candidate candidate-list])
    (let-values ([(match score) (fuzzy-match word candidate threshold)])
      (if (and match (> score best-score))
          (values match score)
          (values best-match best-score)))))


