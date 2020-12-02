#lang racket/base

(require racket/function
         racket/list
         racket/string)

(define (split-by-same xs)
  (define (loop acc xs)
    (if (null? xs)
        (reverse acc)
        (loop (cons (takef xs (curry eq? (car xs))) acc)
              (dropf xs (curry eq? (car xs))))))
  (loop '() xs))

(define (spellout xs)
  ;; assumes all elements are the same
  (format "~a~a" (length xs) (car xs)))

(define (look-and-say xs)
  (define groups (split-by-same (string->list xs)))
  (string-join (map spellout groups) ""))

;; part 1

(for/fold ((acc "1113122113")
           #:result (string-length acc))
          ((_ (in-range 40)))
  (look-and-say acc))

;; part 2

(for/fold ((acc "1113122113")
           #:result (string-length acc))
          ((_ (in-range 50)))
  (look-and-say acc))
