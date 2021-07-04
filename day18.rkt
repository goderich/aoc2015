#lang racket

(require threading)

(define start
  (for/set ((line (file->lines "inputs/day18.txt"))
            (x 100)
            #:when #t ; nested loop
            (char (in-string line))
            (y 100)
            #:when (char=? char #\#))
    (make-rectangular x y)))

(define (neighbours point)
  (for*/list ((x '(-1 0 1))
              (y '(0-1i 0 0+1i))
              #:unless (zero? (+ x y))
              #:when (<= 0 (real-part (+ point x y)) 99)
              #:when (<= 0 (imag-part (+ point x y)) 99))
    (+ point x y)))

(define (step grid #:stuck (stuck #f))
  (define (num-neighbours point)
    (~>> point
         (neighbours)
         (count (λ (x) (set-member? grid x)))))
  (define (switch-on? point)
    (cond
      (else (if (set-member? grid point)
                (<= 2 (num-neighbours point) 3)
                (= 3 (num-neighbours point))))))
  (define new-grid
    (for/set ((point (remove-duplicates (flatten (set-map grid neighbours))))
              #:when (switch-on? point))
      point))
  (if (not stuck)
      new-grid
      ;; Corners always on in part 2
      (~> new-grid
          (set-add 0)
          (set-add 99)
          (set-add 0+99i)
          (set-add 99+99i))))

;; part 1

(for/fold ((grid start)
           #:result (set-count grid))
          ((_ 100))
  (step grid))

;; part 2

(for/fold ((grid start)
           #:result (set-count grid))
          ((_ 100))
  (step grid #:stuck #t))
