#lang racket/base

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         racket/match
         racket/file
         racket/list)

(struct coord (x y) #:transparent)
(struct instruction (flag nw-point se-point) #:transparent)

(define (matrix nw se)
  (for*/list ((x (in-range (coord-x nw) (add1 (coord-x se))))
              (y (in-range (coord-y nw) (add1 (coord-y se)))))
    (coord x y)))

(define (execute! f point)
  (match f
    ('on (hash-set! grid point 1))
    ('off (hash-set! grid point 0))
    ('toggle (hash-update! grid point
                           (λ (x) (if (zero? x) 1 0))))))

(define (run-instruction str handler)
  (define result
    (parse-result!
     (parse-string instruction/p str)))
  (define flag (instruction-flag result))
  (define points (matrix
                  (instruction-nw-point result)
                  (instruction-se-point result)))
  (for ((point points))
    (handler flag point)))

(define on/p
  (do (string/p "on ")
      (pure 'on)))

(define off/p
  (do (string/p "off ")
      (pure 'off)))

(define switch/p
  (do (string/p "turn ")
      (or/p (try/p on/p)
            off/p)))

(define toggle/p
  (do (string/p "toggle ")
      (pure 'toggle)))

(define coord/p
  (do (x <- integer/p)
      (char/p #\,)
      (y <- integer/p)
      (pure (coord x y))))

(define instruction/p
  (do (flag <- (or/p (try/p toggle/p)
                     switch/p))
      (nw-point <- coord/p)
      (string/p " through ")
      (se-point <- coord/p)
      (pure (instruction flag nw-point se-point))))

;; part 1

(define grid
  (make-hash
   (map (λ (x) (cons x 0))
        (matrix (coord 0 0) (coord 999 999)))))

(for ((line (file->lines "inputs/day6.txt")))
  (run-instruction line execute!))

(count positive? (hash-values grid))

;; part 2

(define (execute2! f point)
  (match f
    ('toggle (hash-update! grid point (λ (x) (+ 2 x))))
    ('on (hash-update! grid point add1))
    ('off (hash-update! grid point
                        (λ (x) (if (zero? x)
                                   0
                                   (sub1 x)))))))

;; Reset the grid for part 2
(run-instruction "turn off 0,0 through 999,999" execute!)

(for ((line (file->lines "inputs/day6.txt")))
  (run-instruction line execute2!))

(apply + (hash-values grid))
