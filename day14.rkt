#lang racket

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         data/either)

(struct reindeer
  (name speed flight-time rest-time)
  #:transparent)

(define name/p
  (do (name <- (many/p letter/p))
      (pure (list->string name))))

(define reindeer/p
  (do (name <- name/p)
      (string/p " can fly ")
      (speed <- integer/p)
      (string/p " km/s for ")
      (flight-time <- integer/p)
      (string/p " seconds, but then must rest for ")
      (rest-time <- integer/p)
      (string/p " seconds.")
      eof/p
      (pure (reindeer name speed flight-time rest-time))))

(define reindeer-list
  (for/list ((line (file->lines "inputs/day14.txt")))
    (from-success #f (parse-string reindeer/p line))))

(define (distance-traveled rndr seconds)
  (define flight-time (reindeer-flight-time rndr))
  (define rest-time (reindeer-rest-time rndr))
  (define cycle-time (+ flight-time rest-time))
  (define-values (num-cycles rem)
    (quotient/remainder seconds cycle-time))
  (define rem-flight-time
    (if (rem . > . flight-time)
        flight-time
        rem))
  (* (reindeer-speed rndr)
     (+ (* num-cycles flight-time)
        rem-flight-time)))

;; part 1
(apply max (map (λ (r) (distance-traveled r 2503)) reindeer-list))

;; part 2
(define leaderboard
  (for/hash ((r reindeer-list))
    (values (reindeer-name r) 0)))

(define (find-leaders score-list)
  (define max-score (cdr (argmax cdr score-list)))
  (define (is-max-score? r) (= (cdr r) max-score))
  (map car (filter is-max-score? score-list)))

(for/fold ((scores leaderboard)
           #:result (apply max (hash-values scores)))
          ((seconds (in-inclusive-range 1 2503)))
  (define current-scores
    (map (λ (r) (cons (reindeer-name r) (distance-traveled r seconds)))
         reindeer-list))
  (define current-leaders (find-leaders current-scores))
  (foldl (λ (r acc) (hash-update acc r add1)) scores current-leaders))
