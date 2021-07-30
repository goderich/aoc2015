#lang racket

(struct entity
  (hp damage armor))

(define boss
  (apply entity
         (for/list ((line (file->lines "inputs/day21.txt")))
           (string->number (last (string-split line))))))

(define weapons
  '((dagger        8  4  0)
    (shortsword   10  5  0)
    (warhammer    25  6  0)
    (longsword    40  7  0)
    (greataxe     74  8  0)))

(define armor
  '((none          0  0  0)
    (leather      13  0  1)
    (chainmail    31  0  2)
    (splintmail   53  0  3)
    (bandedmail   75  0  4)
    (platemail   102  0  5)))

(define rings
  '((none         0  0  0)
    (damage+1    25  1  0)
    (damage+2    50  2  0)
    (damage+3   100  3  0)
    (defense+1   20  0  1)
    (defense+2   40  0  2)
    (defense+3   80  0  3)))

(define (calculate-strike-damage attacker defender)
  (if ((entity-armor defender) . >= . (entity-damage attacker))
      1
      ((entity-damage attacker) . - . (entity-armor defender))))

(define (determine-winner player)
  (define player-damage (calculate-strike-damage player boss))
  (define boss-damage (calculate-strike-damage boss player))
  (define player-win-turns (quotient (entity-hp boss) player-damage))
  (define boss-win-turns (quotient (entity-hp player) boss-damage))
  (if (player-win-turns . <= . boss-win-turns)
      'player
      'boss))

(define (sum lst)
  (apply + lst))

;; To increase efficiency, I'm calculating the answers to both parts
;; in a single fold. Both parts require folding over all possible
;; combinations of items, so it's faster to calculate the mins and maxes
;; simultaneously.
;; Since both the armor and the rings are optional, I've added an extra
;; "none" item to both lists. It's the simplest way to code this imo.

(for*/fold ((lowest +inf.0)
            (highest 0))
           ((w weapons)
            (a armor)
            (r1 rings)
            (r2 rings))
  (define items (list w a r1 r2))
  (define player
    (entity 100
            (sum (map third items))
            (sum (map fourth items))))
  (define cost (sum (map second items)))
  (values
   (if (and (eq? 'player (determine-winner player))
            (cost . < . lowest))
       cost
       lowest)
   (if (and (eq? 'boss (determine-winner player))
            (cost . > . highest))
       cost
       highest)))
