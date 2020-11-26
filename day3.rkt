#lang racket/base

(require racket/file
         racket/string
         racket/match)

(define (update-pos c pos)
  (match c
    (#\^ (cons (car pos) (add1 (cdr pos))))
    (#\< (cons (sub1 (car pos)) (cdr pos)))
    (#\> (cons (add1 (car pos)) (cdr pos)))
    (#\v (cons (car pos) (sub1 (cdr pos))))))

;; part 1

(for/fold ((pos '(0 . 0))
           (dict (make-immutable-hash '(((0 . 0) . 1))))
           #:result (length (hash-keys dict)))
          ((c (in-string (string-trim (file->string "inputs/day3.txt")))))
  (define new-pos (update-pos c pos))
  (values new-pos
          (hash-update dict new-pos add1 0)))

;; part 2

(for/fold ((pos1 '(0 . 0))
           (pos2 '(0 . 0))
           (switch #t)
           (dict (make-immutable-hash '(((0 . 0) . 2))))
           #:result (length (hash-keys dict)))
          ((c (in-string (string-trim (file->string "inputs/day3.txt")))))
  (define new-pos (update-pos c (if switch pos1 pos2)))
  (values (if switch new-pos pos1)
          (if switch pos2 new-pos)
          (not switch)
          (hash-update dict new-pos add1 0)))
