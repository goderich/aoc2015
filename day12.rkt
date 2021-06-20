#lang racket

(require threading
         json)

(define input (string-trim (file->string "inputs/day12.txt")))

;; part 1
(~>> input
     (string-split _ #px"[^0-9-]+")
     (map string->number)
     (apply +))

;; part 2
(define json-data
  (with-input-from-string input
    (λ () (read-json))))

(define (traverse-list lst)
  (for/sum ((x lst)
            #:unless (string? x))
    (if (number? x)
        x
        (traverse-json x))))

(define (traverse-json data)
  (cond
    ((and (hash? data)
          (member "red" (hash-values data))) 0)
    ((hash? data) (traverse-list (hash-values data)))
    (else (traverse-list data))))

(traverse-list json-data)
