#lang racket/base

(require openssl/md5)

(define starts-with-zeros
  (λ (i #:numzeros (zs 5))
    (equal? (make-string zs #\0)
            (substring (md5 (open-input-string (format "bgvyzdsv~a" i))) 0 zs))))

;; part 1

(for/first ((i (in-naturals))
            #:when (starts-with-zeros i))
  i)

;; part 2

(for/first ((i (in-naturals))
            #:when (starts-with-zeros i #:numzeros 6))
  i)
