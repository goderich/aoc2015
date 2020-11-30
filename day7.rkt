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
  (cond
    ((member "AND" (string-split instr))
     (bitwise-and (value! (first (string-split instr)))
                  (value! (third (string-split instr)))))
    ((member "OR" (string-split instr))
     (bitwise-ior (value! (first (string-split instr)))
                  (value! (third (string-split instr)))))
    ((member "LSHIFT" (string-split instr))
     (arithmetic-shift (value! (first (string-split instr)))
                       (string->number (third (string-split instr)))))
    ((member "RSHIFT" (string-split instr))
     (arithmetic-shift (value! (first (string-split instr)))
                       (- (string->number (third (string-split instr))))))
    ((member "NOT" (string-split instr))
     (bitwise-xor (value! (second (string-split instr)))
                  65535))
    (else (value! instr))))

(define (value! var)
  (cond
    ((number? var) var)
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
