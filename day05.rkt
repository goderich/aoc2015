#lang racket/base

(require racket/file
         racket/string
         racket/function
         racket/list
         racket/match
         racket/contract
         threading)

;; part 1

(define (has-3-vowels? str)
  (define vowels (string->list "aoeui"))
  (~>> str
       (string->list)
       (filter (λ (c) (member c vowels)))
       (length)
       (<= 3)))

(define (has-double-letter? str)
  (define cs (string->list str))
  (ormap eq?
         (rest cs)
         (drop-right cs 1)))

(define (no-forbidden-strs? str)
  (not
   (ormap (curry string-contains? str)
          '("ab" "cd" "pq" "xy"))))

(define (nice-string? str)
  (and (has-3-vowels? str)
       (has-double-letter? str)
       (no-forbidden-strs? str)))

(count nice-string? (file->lines "inputs/day05.txt"))

;; part 2

(define (has-repeating-pair? str)
  (for/or ((i (in-range (sub1 (string-length str)))))
    (define pattern (substring str i (+ i 2)))
    (string-contains? (substring str (+ i 2)) pattern)))

(define (groups-of-3 lst)
  (if ((length lst) . < . 3)
      '()
      (cons (take lst 3) (groups-of-3 (rest lst)))))

(define (list-len-3? lst)
  (and (list? lst)
       (= 3 (length lst))))

(define/contract (split-pair? lst)
  (list-len-3? . -> . boolean?)
  (match lst
    ((list a _ c) (eq? a c))))

(define (has-split-pair? str)
  (~>> str
       (string->list)
       (groups-of-3)
       (ormap split-pair?)))

(define (nice-string-2? str)
  (and (has-repeating-pair? str)
       (has-split-pair? str)))

(count nice-string-2? (file->lines "inputs/day05.txt"))
