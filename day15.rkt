#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either
         threading
         syntax/parse/define)

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

;; This is really just practice for me, though it does
;; make a few things shorter and the optional #:when
;; more elegant. I wouldn't say it's more readable than
;; a function with a clumsy check on the when.
(define-syntax-parser find-max-score
  ;; Need to specify :id here so that the macro
  ;; doesn't eat the #:calories? keyword and the integer.
  ((_ var:id ... (~optional (~seq #:calories? cal)))
   #'(for*/fold ((score 0))
                ((var (in-inclusive-range 0 100)) ...
                #:when (= 100 (+ var ...))
                ;; Additional condition for part 2
                (~? (~@ #:when (= cal (apply + (map * (list var ...) (map fifth props)))))))
       (define new-score (calculate-score (list var ...) props))
       (if (new-score . > . score)
           new-score
           score))))

;; part 1
(find-max-score capacity durability flavour texture)

;; part 2
(find-max-score capacity durability flavour texture #:calories? 500)

;; Unfolds into:
;;
;; (for*/fold
;;  ((score 0))
;;  ((capacity   (in-inclusive-range 0 100))
;;   (durability (in-inclusive-range 0 100))
;;   (flavour    (in-inclusive-range 0 100))
;;   (texture    (in-inclusive-range 0 100))
;;   #:when (= 100 (+ capacity durability flavour texture))
;;   #:when (= 500 (apply +
;;                   (map *
;;                     (list capacity durability flavour texture)
;;                     (map fifth props)))))
;;  (define new-score
;;    (calculate-score (list capacity durability flavour texture) props))
;;  (if (> new-score score) new-score score))
