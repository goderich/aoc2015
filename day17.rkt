#lang racket

(define containers
  (map string->number (file->lines "inputs/day17.txt")))

;; Main takeaway: use `in-combinations` instead of `combinations`
;; whenever possible. So much faster.

;; part 1

;; There is also sequence-count, but it's a little slower
;; and requires a lambda or separate function.
;; It may be used here, but it's less fitting for part 2.
(for/fold ((acc 0))
          ((cs (in-combinations containers))
           #:when (= 150 (apply + cs)))
  (add1 acc))

;; part 2

;; I could combine this iteration with the previous one,
;; but it already runs quite fast, so I'll keep them
;; separate for legibility.
(define min-containers
  (for/fold ((curr-min +inf.0))
            ((cs (in-combinations containers))
             #:when (= 150 (apply + cs)))
    (define num-conts (length cs))
    (if (num-conts . < . curr-min)
        num-conts
        curr-min)))

(for/fold ((acc 0))
          ((cs (in-combinations containers))
           #:when (= min-containers (length cs))
           #:when (= 150 (apply + cs)))
    (add1 acc))
