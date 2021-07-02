#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either
         syntax/parse/define)

(struct sue (number compounds))

;; Just practicing me macro skills
(define-syntax-parse-rule (try-strings/p str ...)
   (or/p (try/p (string/p str)) ...))

(define compound/p
  (do (c <- (try-strings/p "children" "cats"
                           "samoyeds" "pomeranians"
                           "akitas" "vizslas"
                           "goldfish" "trees"
                           "cars" "perfumes"))
      (char/p #\:)
      space/p
      (n <- integer/p)
      (pure (cons c n))))

(define sue/p
  (do (string/p "Sue ")
      (num <- integer/p)
      (char/p #\:)
      space/p
      (cs <- (many/p compound/p #:sep (string/p ", ")))
      (pure (sue num (make-hash cs)))))

(define input
  (for/list ((line (file->lines "inputs/day16.txt")))
    (from-success #f (parse-string sue/p line))))

(define sue-gift
  #hash(("children"    . 3)
        ("cats"        . 7)
        ("samoyeds"    . 2)
        ("pomeranians" . 3)
        ("akitas"      . 0)
        ("vizslas"     . 0)
        ("goldfish"    . 5)
        ("trees"       . 3)
        ("cars"        . 2)
        ("perfumes"    . 1)))

;; part 1

(define (correct-sue? s)
  (define (correct-amount? compound num)
    (= num (hash-ref sue-gift compound)))
  (for/and (((compound num) (sue-compounds s)))
    (correct-amount? compound num)))

(for/first ((s input)
            #:when (correct-sue? s))
  (sue-number s))

;; part 2

(define (correct-sue-2? s)
  (define (correct-amount? compound num)
    (match compound
      ((or "cats" "trees") (num . > .(hash-ref sue-gift compound)))
      ((or "pomeranians" "goldfish") (num . < .(hash-ref sue-gift compound)))
      (_ (= num (hash-ref sue-gift compound)))))
  (for/and (((compound num) (sue-compounds s)))
    (correct-amount? compound num)))

(for/first ((s input)
            #:when (correct-sue-2? s))
  (sue-number s))
