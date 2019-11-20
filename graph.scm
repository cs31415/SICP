#lang racket

(define (make-vertex key adj-list)
  (list key adj-list))

(define (get-key vertex)
  (car vertex))

(define (get-adj-list vertex)
  (cadr vertex))

(define (make-adj-list . keys)
  keys)

(define (make-graph . vertices)
  vertices)

(define (get-vertices graph)
  graph)

(define v1 (make-vertex 1 (make-adj-list 2 4)))
(define v2 (make-vertex 2 (make-adj-list 5)))
(define v3 (make-vertex 3 (make-adj-list 6 5)))
(define v4 (make-vertex 4 (make-adj-list 2)))
(define v5 (make-vertex 5 (make-adj-list 4)))
(define v6 (make-vertex 6 (make-adj-list 6)))

(define g (make-graph v1 v2 v3 v4 v5 v6))
g

(define h (hash 1 "one" 2 "two"))
(hash-ref h 1)

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))
 
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;(apply hash (flatmap (lambda (x) (list (get-key x) 0)) g))

(define (foreach l f)
  (if (null? l)
      (void)
      (begin
        (f (first l))
        (foreach (rest l) f))))

(define (init-hash l f v)
  (let ((h (make-hash)))
    (begin
      (foreach l (lambda (x) (hash-set! h (f x) v)))
      h)))

(define (bfs g skey)
  (let
    ((q '())
     (dist (init-hash g (lambda (x) (car x)) -1))
     (pred (init-hash g (lambda (x) (car x)) nil))
     (color (init-hash g (lambda (x) (car x)) "white")))
    ))
    
    
    
    