#lang racket/base

(require racket/file
         racket/string
         racket/match)

(define (update-pos c pos)
  (match-define (cons x y) pos)
  (match c
    (#\^ (cons x (add1 y)))
    (#\< (cons (sub1 x) y))
    (#\> (cons (add1 x) y))
    (#\v (cons x (sub1 y)))))

;; part 1

(for/fold ((pos '(0 . 0))
           (dict (make-immutable-hash '(((0 . 0) . 1))))
           #:result (length (hash-keys dict)))
          ((c (in-string (string-trim (file->string "inputs/day03.txt")))))
  (define new-pos (update-pos c pos))
  (values new-pos
          (hash-update dict new-pos add1 0)))

;; part 2

(for/fold ((pos1 '(0 . 0))
           (pos2 '(0 . 0))
           (switch #t)
           (dict (make-immutable-hash '(((0 . 0) . 2))))
           #:result (length (hash-keys dict)))
          ((c (in-string (string-trim (file->string "inputs/day03.txt")))))
  (define new-pos (update-pos c (if switch pos1 pos2)))
  (values (if switch new-pos pos1)
          (if switch pos2 new-pos)
          (not switch)
          (hash-update dict new-pos add1 0)))
