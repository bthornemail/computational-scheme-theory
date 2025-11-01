#!/usr/bin/env python3
"""
Generate Test Corpus for Computational Scheme Theory Validation

Generates R5RS Scheme programs organized into multiple categories
as specified in docs/10 - IMPLEMENTATION/04-test-corpus-design.md
"""

import os
from pathlib import Path

BASE_DIR = Path(__file__).parent.parent / "test-corpus"

# Baseline programs (20 total) - Straight-line code
BASELINE_PROGRAMS = [
    ("B001", "(define (add-three x) (+ x 3))"),
    ("B002", """(define (compute x y)
  (let ((a (+ x y))
        (b (* x y)))
    (/ a b)))"""),
    ("B003", """(define (nested x)
  (let ((a x))
    (let ((b (+ a 1)))
      (let ((c (+ b 1)))
        c))))"""),
    ("B004", "(define identity (lambda (x) x))"),
    ("B005", """(define pi 3.14159)
(define e 2.71828)
(define golden-ratio 1.61803)"""),
    ("B006", "(define (square x) (* x x))"),
    ("B007", """(define (double x) (+ x x))"""),
    ("B008", """(define (quadruple x)
  (let ((doubled (double x)))
    (+ doubled doubled)))"""),
    ("B009", "(define (multiply x y z) (* x (* y z)))"),
    ("B010", "(define (chain x) (+ x 1))"),
    ("B011", """(define (add x y) (+ x y))"""),
    ("B012", """(define (subtract x y) (- x y))"""),
    ("B013", """(define (multiply x y) (* x y))"""),
    ("B014", """(define (divide x y) (/ x y))"""),
    ("B015", """(define (compose f g)
  (lambda (x) (f (g x))))"""),
    ("B016", """(define (sum-list lst)
  (if (null? lst) 0
      (+ (car lst) (sum-list (cdr lst)))))"""),
    ("B017", """(define (product x y)
  (* x y))"""),
    ("B018", """(define (identity x) x)"""),
    ("B019", """(define (constant x) 42)"""),
    ("B020", """(define (apply-twice f x)
  (f (f x)))"""),
]

# Simple Control programs (50 total) - Single branching/looping
SIMPLE_CONTROL_PROGRAMS = [
    ("SC001", """(define (abs x)
  (if (< x 0)
      (- x)
      x))"""),
    ("SC002", """(define (max a b)
  (if (> a b) a b))"""),
    ("SC003", """(define (min a b)
  (if (< a b) a b))"""),
    ("SC004", """(define (sign x)
  (if (< x 0) -1
      (if (= x 0) 0 1)))"""),
    ("SC005", """(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))"""),
    ("SC006", """(define (even? n)
  (if (= n 0) #t
      (if (= n 1) #f
          (even? (- n 2)))))"""),
    ("SC007", """(define (gcd a b)
  (if (= b 0) a
      (gcd b (modulo a b))))"""),
    ("SC008", """(define (power base exp)
  (if (= exp 0) 1
      (* base (power base (- exp 1)))))"""),
    ("SC009", """(define (fibonacci n)
  (if (< n 2) n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))"""),
    ("SC010", """(define (is-zero? x)
  (if (= x 0) #t #f))"""),
    ("SC011", """(define (positive? x)
  (if (> x 0) #t #f))"""),
    ("SC012", """(define (negative? x)
  (if (< x 0) #t #f))"""),
    ("SC013", """(define (compare a b)
  (if (> a b) 1
      (if (< a b) -1 0)))"""),
    ("SC014", """(define (absolute-diff a b)
  (if (> a b)
      (- a b)
      (- b a)))"""),
    ("SC015", """(define (clamp x min max)
  (if (< x min) min
      (if (> x max) max x)))"""),
]

# Recursion programs (50 total) - Recursive functions
RECURSION_PROGRAMS = [
    ("R001", """(define (fact n)
  (if (= n 0) 1
      (* n (fact (- n 1)))))"""),
    ("R002", """(define (sum-to-n n)
  (if (= n 0) 0
      (+ n (sum-to-n (- n 1)))))"""),
    ("R003", """(define (list-length lst)
  (if (null? lst) 0
      (+ 1 (list-length (cdr lst)))))"""),
    ("R004", """(define (reverse-list lst)
  (if (null? lst) '()
      (append (reverse-list (cdr lst))
              (list (car lst)))))"""),
    ("R005", """(define (map f lst)
  (if (null? lst) '()
      (cons (f (car lst))
            (map f (cdr lst)))))"""),
    ("R006", """(define (filter pred lst)
  (if (null? lst) '()
      (if (pred (car lst))
          (cons (car lst) (filter pred (cdr lst)))
          (filter pred (cdr lst)))))"""),
    ("R007", """(define (fold-left op init lst)
  (if (null? lst) init
      (fold-left op (op init (car lst)) (cdr lst))))"""),
    ("R008", """(define (take n lst)
  (if (or (= n 0) (null? lst)) '()
      (cons (car lst) (take (- n 1) (cdr lst)))))"""),
    ("R009", """(define (drop n lst)
  (if (or (= n 0) (null? lst)) lst
      (drop (- n 1) (cdr lst))))"""),
    ("R010", """(define (member? x lst)
  (if (null? lst) #f
      (if (equal? x (car lst)) #t
          (member? x (cdr lst)))))"""),
    ("R011", """(define (append-lists lst1 lst2)
  (if (null? lst1) lst2
      (cons (car lst1)
            (append-lists (cdr lst1) lst2))))"""),
    ("R012", """(define (count-items x lst)
  (if (null? lst) 0
      (+ (if (equal? x (car lst)) 1 0)
         (count-items x (cdr lst)))))"""),
    ("R013", """(define (all? pred lst)
  (if (null? lst) #t
      (if (pred (car lst))
          (all? pred (cdr lst))
          #f)))"""),
    ("R014", """(define (any? pred lst)
  (if (null? lst) #f
      (if (pred (car lst)) #t
          (any? pred (cdr lst)))))"""),
    ("R015", """(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (list (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))"""),
]

# Complex Control programs (50 total) - Nested structures
COMPLEX_CONTROL_PROGRAMS = [
    ("CC001", """(define (nested-if x y z)
  (if (> x 0)
      (if (> y 0)
          (if (> z 0) (+ x y z) x)
          y)
      z))"""),
    ("CC002", """(define (multi-branch x)
  (if (< x 0) -1
      (if (= x 0) 0
          (if (< x 10) 1
              2))))"""),
    ("CC003", """(define (complex-logic a b c)
  (if (and (> a 0) (> b 0))
      (if (> c 0) (+ a b c)
          (* a b))
      (if (< c 0) (- a b)
          0)))"""),
    ("CC004", """(define (recursive-nested n)
  (if (< n 2) n
      (if (= (modulo n 2) 0)
          (recursive-nested (/ n 2))
          (recursive-nested (- n 1)))))"""),
    ("CC005", """(define (nested-factorial n)
  (if (= n 0) 1
      (if (= n 1) 1
          (* n (nested-factorial (- n 1))))))"""),
    ("CC006", """(define (conditional-sum lst)
  (if (null? lst) 0
      (if (> (car lst) 0)
          (+ (car lst) (conditional-sum (cdr lst)))
          (conditional-sum (cdr lst)))))"""),
    ("CC007", """(define (triple-nested x)
  (if (> x 0)
      (if (> x 10)
          (if (> x 100) 3 2)
          1)
      0))"""),
    ("CC008", """(define (complex-filter pred lst)
  (if (null? lst) '()
      (if (pred (car lst))
          (if (pred (cdr lst))
              (cons (car lst) (complex-filter pred (cdr lst)))
              (list (car lst)))
          (complex-filter pred (cdr lst)))))"""),
    ("CC009", """(define (multi-path x y)
  (if (< x 0)
      (if (< y 0) 'both-negative 'x-negative)
      (if (< y 0) 'y-negative 'both-positive)))"""),
    ("CC010", """(define (nested-recursion n)
  (if (< n 1) 0
      (if (= n 1) 1
          (+ (nested-recursion (- n 1))
             (nested-recursion (- n 2))))))"""),
]

# Functional programs (50 total) - Higher-order functions
FUNCTIONAL_PROGRAMS = [
    ("F001", """(define (compose f g)
  (lambda (x) (f (g x))))"""),
    ("F002", """(define (curry f)
  (lambda (x) (lambda (y) (f x y))))"""),
    ("F003", """(define (apply-twice f x)
  (f (f x)))"""),
    ("F004", """(define (map f lst)
  (if (null? lst) '()
      (cons (f (car lst))
            (map f (cdr lst)))))"""),
    ("F005", """(define (filter pred lst)
  (if (null? lst) '()
      (if (pred (car lst))
          (cons (car lst) (filter pred (cdr lst)))
          (filter pred (cdr lst)))))"""),
    ("F006", """(define (fold-right op init lst)
  (if (null? lst) init
      (op (car lst)
          (fold-right op init (cdr lst)))))"""),
    ("F007", """(define (partial f x)
  (lambda (y) (f x y)))"""),
    ("F008", """(define (chain f g h)
  (lambda (x) (h (g (f x)))))"""),
    ("F009", """(define (flip f)
  (lambda (x y) (f y x)))"""),
    ("F010", """(define (iterate f n x)
  (if (= n 0) x
      (iterate f (- n 1) (f x))))"""),
]

def write_program(category, filename, code):
    """Write a program to the appropriate category directory"""
    category_dir = BASE_DIR / category
    category_dir.mkdir(parents=True, exist_ok=True)
    filepath = category_dir / f"{filename}.scm"
    with open(filepath, 'w') as f:
        f.write(code + "\n")
    return filepath

def generate_corpus():
    """Generate the test corpus"""
    print("Generating expanded test corpus...")
    
    categories = [
        ("baseline", BASELINE_PROGRAMS),
        ("simple-control", SIMPLE_CONTROL_PROGRAMS),
        ("recursion", RECURSION_PROGRAMS),
        ("complex-control", COMPLEX_CONTROL_PROGRAMS),
        ("functional", FUNCTIONAL_PROGRAMS),
    ]
    
    total = 0
    for category, programs in categories:
        print(f"\n  Generating {len(programs)} {category} programs...")
        for name, code in programs:
            write_program(category, name, code)
            total += 1
    
    print(f"\n✅ Generated {total} test programs across {len(categories)} categories")
    print(f"   Categories: {', '.join([c[0] for c in categories])}")

if __name__ == "__main__":
    generate_corpus()
