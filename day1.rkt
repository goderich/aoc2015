#lang racket/base

(require racket/file
         racket/match)

(define (par-to-fun c acc)
  (match c
    (#\( (add1 acc))
    (#\) (sub1 acc))
    (_ acc)))

;; part 1
(foldl par-to-fun 0
       (string->list (file->string "inputs/day1.txt")))

;; part 2
(for/fold ((acc 0)
           (i 0)
           #:result i)
          ((c (in-string (file->string "inputs/day1.txt")))
           #:break (= -1 acc))
  (values (par-to-fun c acc) (add1 i)))
