#lang racket/base

(require racket/file
         racket/string
         racket/list
         threading)

(define origins-distances
 (for/list ((line (file->lines "inputs/day9.txt")))
   (define words (string-split line))
   (define origin (first words))
   (define distance (string->number (last words)))
   (cons origin distance)))

(define origins
  (remove-duplicates (map car origins-distances)))

(for/list ((city origins))
  (filter (λ (x) (equal? city (car x))) origins-distances))
