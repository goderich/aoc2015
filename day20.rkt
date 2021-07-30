#lang racket

(require math/number-theory)

(define input
  (string->number
   (string-trim
    (file->string "inputs/day20.txt"))))

;; part 1
(for/first ((house (in-naturals))
            #:when ((* 10 (apply + (divisors house))) . >= . input))
  house)

;; part 2
(define (num-gifts house)
  (define divs (divisors house))
  (* 11
     (for/sum ((d divs)
               (r (reverse divs))
               #:when (r . <= . 50))
       d)))

(for/first ((house (in-naturals))
            #:when ((num-gifts house) . >= . input))
  house)
