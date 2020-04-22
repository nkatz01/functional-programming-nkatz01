 #lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; This implements a simple desk calculator in Racket.

;; Every calculator needs memory!
(define memory (make-parameter 0))
(define (mem-add! x)
  (memory (+ x (memory)))
  x)
(define (mem-subtract! x)
  (memory (- (memory) x))
  x)
(define (memory-clear!) (memory 0))


;; for more information, see parser-tools/examples/calc.rkt
;; Lexing: Defining tokens. (boilerplate)
(define-tokens value-tokens (NUM))
(define-empty-tokens op-tokens (= + - * / sqrt MR M+ M- MC ± C CE EOF))
(define-lex-abbrevs
 (digit (:/ "0" "9")))

(define lex-token
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space #\newline)
    (lex-token input-port)]
   [(:or "=" "+" "-" "*" "/" "*" "sqrt" "MR" "M+" "M-" "MC" "±" "C" "CE")
    (string->symbol lexeme)]
   [(:: (:+ digit) (:? ".") (:* digit))
    (token-NUM (string->number lexeme))]))

(define calc-parser
  (parser
   (start calc)
   (end EOF)
   (tokens value-tokens op-tokens)
   (suppress) ;; Our grammar is ambiguous. By default, parser complains.
   (error (λ(ok? name value) (printf "Couldn't parse: ~a\n" name)))

   (grammar
    (calc [(datum)      (list $1)]
          [(datum calc) (cons $1 $2)])

    (datum [(C)         #f]
           [(MC)        #f]
           [(CE)        #f]
           [(expr =)    $1]
           [(expr = M+) (mem-add! $1)]
           [(expr = M-) (mem-subtract! $1)]
           [(expr = MC) (begin (memory-clear!) $1)]
           [(broken-expr) #f])

    (expr [(expr + term) (+ $1 $3)]
          [(expr - term) (- $1 $3)]
          [(term)        $1])

    (term [(term * val) (* $1 $3)]
          [(term / val) (/ $1 $3)]
          [(val)        $1])

    (val [(val CE val) $3]
         [(val sqrt)   (sqrt $1)]
         [(MR)         (memory)]
         [(val ±)      (- $1)]
         [(NUM)        $1])

    (broken-expr [(expr C)   0]
                 [(expr + C) 0]
                 [(expr - C) 0]
                 [(term C)   0]
                 [(term * C) 0]
                 [(term / C) 0]
                 [(val C)    0]
                 [(val CE C) 0]))))

(define (calc str)
  (let* ([port (open-input-string str)]
         [results (calc-parser (λ() (lex-token port)))])
    (for ([result (in-list results)])
         (when result
           (displayln result))))
  )

(calc "1 + 2")