#lang racket/base

(struct coord (x y) #:transparent)

(define (matrix nw se)
  (for*/list ((x (in-range (coord-x nw) (add1 (coord-x se))))
              (y (in-range (coord-y nw) (add1 (coord-y se)))))
    (coord x y)))

(define grid
  (make-hash
   (map (λ (x) (cons x 0))
        (matrix (coord 0 0) (coord 999 999)))))

