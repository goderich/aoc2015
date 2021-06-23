#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either
         threading)

;; Megaparsack doesn't include facilities to parse
;; negative integers, so this is a simple parser for
;; negative and positive ints.
(define int/p
  (do (m <- (many/p (char/p #\-) #:max 1))
      (i <- integer/p)
      (pure (if (empty? m) i (- i)))))

(define properties/p
  (do (many/p letter/p)
      (char/p #\:)
      (string/p " capacity ")
      (cap <- int/p)
      (string/p ", durability ")
      (dur <- int/p)
      (string/p ", flavor ")
      (fla <- int/p)
      (string/p ", texture ")
      (tex <- int/p)
      (string/p ", calories ")
      (cal <- int/p)
      eof/p
      (pure (list cap dur fla tex cal))))

(define props
  (for/list ((line (file->lines "inputs/day15.txt")))
    (from-success #f (parse-string properties/p line))))

(define (calculate-score amounts property-list)
  (define (calculate-property-score f)
    (define interim-score
      (~>> property-list
           (map f)
           (map * amounts)
           (apply +)))
    (if (interim-score . < . 0)
        0
        interim-score))
  (for/product ((f (list first second third fourth)))
    (calculate-property-score f)))

(define (find-max-score #:calories? (cal? #f))
  (for*/fold ((score 0))
             ((a (in-inclusive-range 0 100))
              (b (in-inclusive-range 0 100))
              (c (in-inclusive-range 0 100))
              (d (in-inclusive-range 0 100))
              #:when (and (= 100 (+ a b c d))
                          ;; Additional condition for part 2
                          (if cal?
                              (= 500 (apply + (map * (list a b c d) (map fifth props))))
                              #t)))
    (define new-score (calculate-score (list a b c d) props))
    (if (new-score . > . score)
        new-score
        score)))

(find-max-score)
(find-max-score #:calories? #t)
