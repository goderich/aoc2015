#lang racket/base

(require racket/file
         racket/port
         racket/system
         racket/string)

;; part 1

(define string-literals-num
  (for/sum ((s (file->lines "inputs/day08.txt")))
    (string-length s)))

(define string-values-num
  (for/sum ((s (file->lines "inputs/day08.txt")))
    (string-length
     (with-input-from-string s read))))

(- string-literals-num string-values-num)

;; part 2
;; I'm not sure if Racket has provisions for such perversions
;; with strings, but in any case it's a lot easier to simply
;; count the number of \ and " in the file (since each has to
;; be escaped exactly once) using command-line tools, and then
;; add 2 times the number of lines for the new surrounding quotes.

(+
 (* 2 (length (file->lines "inputs/day8.txt")))
 (string->number
  (string-trim
   (with-output-to-string
     (λ _
       (system "grep -o -E '(\\\\|\")' /home/goderich/git/aoc2015/inputs/day8.txt | wc -l"))))))
