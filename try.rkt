#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
;(print (port-count-lines! (open-input-file "cmmExamples/example5.cmm")))

(define frs (cdr (string->list "x=1\n(x + 2 * 3) - (1+2)*3")))
 (display frs)