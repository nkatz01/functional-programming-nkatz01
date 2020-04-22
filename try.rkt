#lang racket/base
 
 
(require brag/support)
 
;(print (port-count-lines! (open-input-file "cmmExamples/example5.cmm")))
 
(require "nested-word-list.rkt")
(define a-parsed-value
    (parse (list (token 'LEFT-PAREN "(")
                 (token 'WORD "some")
                 (token 'LEFT-PAREN "[")
                 (token 'WORD "pig")
                 (token 'RIGHT-PAREN "]")
                 (token 'RIGHT-PAREN ")"))))

 