#lang racket/base

(require racket/file
         racket/match
         racket/list
         racket/string)

(define (read-instr-line line)
  (match-define (cons instr var-lst) (string-split line " -> "))
  (define var (car var-lst))
  (hash-set! dict var instr))

(define (calculate-instr instr)
  (define instr-list (string-split instr))
  (cond
    ((member "AND" instr-list)
     (bitwise-and (value! (first instr-list))
                  (value! (third instr-list))))
    ((member "OR" instr-list)
     (bitwise-ior (value! (first instr-list))
                  (value! (third instr-list))))
    ((member "LSHIFT" instr-list)
     (arithmetic-shift (value! (first instr-list))
                       (string->number (third instr-list))))
    ((member "RSHIFT" instr-list)
     (arithmetic-shift (value! (first instr-list))
                       (- (string->number (third instr-list)))))
    ((member "NOT" instr-list)
     (bitwise-xor (value! (second instr-list))
                  65535))
    (else (value! instr))))

(define (value! var)
  (cond
    ((numstr? var) (string->number var))
    (else (define val (calculate-instr (hash-ref dict var)))
          (hash-set! dict var (number->string val))
          val)))

(define (numstr? s)
  (for/and ((c (in-string s)))
    (char-numeric? c)))

;; part 1

(define dict (make-hash))

(for ((line (file->lines "inputs/day7.txt")))
  (read-instr-line line))

(calculate-instr (hash-ref dict "a"))

;; part 2

(hash-clear! dict)

(for ((line (file->lines "inputs/day7.txt")))
  (read-instr-line line))

(hash-set! dict "b" "16076")

(calculate-instr (hash-ref dict "a"))
