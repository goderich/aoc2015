#lang racket/base

(require racket/file
         racket/string
         racket/list
         threading)

(define edges
 (for/list ((line (file->lines "inputs/day9.txt")))
   (define words (string-split line))
   (define origin (first words))
   (define destination (third words))
   (define distance (string->number (last words)))
   (list origin destination distance)))

(define vertices
  (~>> edges
       (map car)
       (remove-duplicates)
       (cons (second (last edges)))))

(define (find-edge v1 v2)
  (for/first ((edge edges)
              #:when (and (member v1 edge)
                          (member v2 edge)))
    (last edge)))

(define distances
  (for/list ((vs (permutations vertices)))
    (for/sum ((v1 (drop-right vs 1))
              (v2 (rest vs)))
      (find-edge v1 v2))))

;; part 1

(apply min distances)

;; part 2

(apply max distances)
