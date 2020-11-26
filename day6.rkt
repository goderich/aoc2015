#lang racket/base

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative)

(struct coord (x y) #:transparent)

(define (matrix nw se)
  (for*/list ((x (in-range (coord-x nw) (add1 (coord-x se))))
              (y (in-range (coord-y nw) (add1 (coord-y se)))))
    (coord x y)))

(define grid
  (make-hash
   (map (λ (x) (cons x 0))
        (matrix (coord 0 0) (coord 999 999)))))

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

(define matrix/p
  (pure 'TODO))

(define instruction/p
  (do (flag <- (or/p (try/p toggle/p)
                     switch/p))
      (block <- (matrix/p))
    (pure)))
