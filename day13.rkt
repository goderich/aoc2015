#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either)

(define word/p
  (do (word <- (many/p letter/p))
      (pure (list->string word))))

(define sign/p
  (do (sign <- (or/p (string/p "gain ")
                     (string/p "lose ")))
      (pure (if (string=? sign "gain ")
                +
                -))))

(define line/p
  (do (name <- word/p)
      (string/p " would ")
      (sign <- sign/p)
      (points <- integer/p)
      (string/p " happiness units by sitting next to ")
      (obj <- word/p)
      (char/p #\.)
      eof/p
      (pure (list name obj (sign points)))))

(define lines
  (for/list ((line (file->lines "inputs/day13.txt")))
    (from-success #f (parse-string line/p line))))

(define guests '("Alice" "Bob" "Carol" "David" "Eric" "Frank" "George" "Mallory"))

(define seating-preferences
  ;; adding "me" for part 2
  (for/hash ((guest (cons "me" guests)))
    (values
     guest
     (for/hash ((line (filter (λ (x) (string=? guest (first x))) lines)))
       (values (second line) (third line))))))

(define (happiness-pair x y)
  ;; adding 0 for part 2
  (+ (hash-ref (hash-ref seating-preferences x) y 0)
     (hash-ref (hash-ref seating-preferences y) x 0)))

(define (total-happiness lst)
  (for/sum ((i (length lst)))
    (define pair
      (if (i . < . (sub1 (length lst)))
          (take (drop lst i) 2)
          (list (last lst) (first lst))))
    (apply happiness-pair pair)))

(define (maximum-happiness guest-list)
  (for/fold ((maximum 0))
            ((lst (in-permutations guest-list)))
    (define happiness (total-happiness lst))
    (if (happiness . > . maximum)
        happiness
        maximum)))

;; part 1
(maximum-happiness guests)

;; part 2
(maximum-happiness (cons "me" guests))
