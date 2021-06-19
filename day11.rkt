#lang racket

(require threading)

(define input (string-trim (file->string "inputs/day11.txt")))

(define (increasing-straight? charlist)
  (= (+ 2 (char->integer (first charlist)))
     (+ 1 (char->integer (second charlist)))
     (char->integer (third charlist))))

(define (satisfies-req-1? password)
  (define charlist (string->list password))
  (for/first ((i 6)
              #:when (increasing-straight? (drop charlist i)))
    #t))

(define (satisfies-req-2? password)
  (not (ormap (curry string-contains? password) '("i" "o" "l"))))

(define (satisfies-req-3? password)
  (define (loop acc charlist)
    (cond
      ((null? charlist) acc)
      (else
       (define-values (straight xs) (splitf-at charlist (curry eq? (first charlist))))
       (loop (cons straight acc) xs))))
  (~>> password
       (string->list)
       (loop '())
       (filter (λ (xs) ((length xs) . >= . 2)))
       (length)
       (<= 2)))

(define (increment password)
  ;; Need to make a mutable copy of the string first.
  (define str (apply string (string->list password)))
  (define (loop i)
    (define curr-char (string-ref str i))
    (if (char=? curr-char #\z)
        (begin
          (string-set! str i #\a)
          (loop (sub1 i)))
        (string-set! str i (integer->char
                            (add1
                             (char->integer curr-char))))))
  (loop 7)
  ;; string-set! returns void, and so does loop.
  ;; We need to explicitly return the string.
  str)

(define (find-next-password password)
  (if (and (satisfies-req-1? password)
           (satisfies-req-2? password)
           (satisfies-req-3? password))
      password
      (find-next-password (increment password))))

;; Answer to part 1
(define part1-answer (find-next-password input))
part1-answer

;; Answer to part 2
(find-next-password (increment part1-answer))
