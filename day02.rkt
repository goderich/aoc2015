#lang racket/base

(require racket/file
         racket/string
         racket/list
         threading)

;; part 1

(define (parse-dimensions str)
  (~>> str
       (string-split _ "x")
       (map string->number)))

(define (calculate-paper ns)
  ;; 2*l*w + 2*w*h + 2*h*l
  (define sides
    (~>> ns
         (combinations _ 2)
         (map (λ (n) (apply * n)))))
  (define extra (apply min sides))
  (define surface-area (* 2 (apply + sides)))
  (+ surface-area extra))

(for/fold ((acc 0))
          ((s (file->lines "inputs/day02.txt")))
  (+ acc (calculate-paper (parse-dimensions s))))

;; part 2

(define (smallest-perimeter ns)
  (~>> ns
       (sort _ <)
       (take _ 2)
       (apply +)
       (* 2)))

(define (calculate-ribbon ns)
  (+ (apply * ns) (smallest-perimeter ns)))

(for/fold ((acc 0))
          ((s (file->lines "inputs/day02.txt")))
  (+ acc (calculate-ribbon (parse-dimensions s))))
