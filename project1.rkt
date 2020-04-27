 #lang racket

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
 (provide interpret)

;https://github.com/racket/parser-tools/blob/master/parser-tools-lib/parser-tools/examples/calc.rkt
;https://gist.github.com/gcr/1318240

#| Disclaimer: I have looked at Sandre Sjursen's code (and where I've copied an idea from him, I shall point this out) and he also helped me understand the original git hub lexer and parser code
by explainng to me what some of the code was doing but mainly he gave me the insight of how one could implement the rest of
the program's statements by having the 'start' point to a 'statement' block in the same way the original example had the start point to an expression block. Though, I would like to clarify, that
as far as I'm aware, he's used nothing of mine and wasn't in need of doing so either. Also I've posted the links above where it's obvious where I've copied my modifed parser and lexer versions from|#





(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR  ))
(define-empty-tokens op-tokens ( newline  = OC CC DEL OP CP + - * / || %   or && == != >= <= > <  EOF PRINT WHILE IF ELSE  ))
 
(define vars (make-hash))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))
 
(define calcl
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space  #\return #\newline  ) (calcl input-port)]
   [ (:= 2  #\|)   (token-||)]
   [(:or "=" "+" "-" "*" "/" "%" "&&"      "==" "!=" ">=" "<=" ">" "<") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["{" 'OC]
   ["}" 'CC]
   [ "print" 'PRINT   ]
   [#\,  'DEL  ]
   [ "while" 'WHILE ]
   [ "if" 'IF ]
   [ "else" 'ELSE ] 
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   ))
   
(define (string->char s)  (car (string->list s)))

(define calcp
  (parser
   (start   start)
   (end   EOF)
   (tokens value-tokens op-tokens  )
   (error (λ(ok? name value) (if (boolean? value) (printf "Couldn't parse: ~a\n" name) (printf "Couldn't parse: ~a\n" value)))) 
   (precs   
    (left DEL);from lowest to highest
    (right =)             
    (left  ||)
    (left &&)
    (left == !=)
    (left <= >= < >)
    (left - +)
    (left * / %)
    (right OP)
    (left CP)
    (right OC)
    (left CC)
    )
   
   (grammar    
    (start
     [() '()]  ; returns empty list when it mathces onto nothing              
     [(statements) `(,$1)]
     [(statements start) `(,$1,$2)]
     )

    (statements
                
     [(var = exp ) `(assign ,$1 ,$3)]
     [(IF ifState) $2]
     [(WHILE while) $2]
     [(PRINT printVals)  `(print ,$2)]
               
                 
                
              
     )
 
    (ifState ; It's assumed that you cannot have an if inside an if unless it's within curly braces
  
     [(OP exp CP statements) `(if ,$2 ,$4)]
     [(OP exp CP block) `(if ,$2 ,$4)]       
     [(OP exp CP block ELSE statements ) `(if ,$2 ,$4 ,$6)]
     [(OP exp CP statements ELSE block ) `(if ,$2 ,$4 ,$6)]
     [(OP exp CP statements ELSE statements ) `(if ,$2 ,$4 ,$6)]       
     [(OP exp CP block ELSE block ) `(if ,$2 ,$4 ,$6)]
     )
    (while
     [(OP exp CP block) `(while ,$2, $4)]
     )

    (block
     [(OC start CC) $2]
     )
   
     
    (var
     [(VAR) $1]
     )
     
    (printVals
          
     [(exp DEL printVals ) `(,$1 ,$3)]
     [(exp) $1]          
         
         
     )
     
        
    (exp [(NUM)  $1]
         [(VAR)  $1] 
         [(exp || exp) `((lambda (a b) (or a b))  ,$1 ,$3) ]  
         [(exp && exp) `((lambda (a b) (and a b)) ,$1 ,$3)]
         [(exp == exp) `(equal? ,$1 ,$3)]
         [(exp != exp) `(not(equal? ,$1 ,$3))]
         [(exp < exp) `(< ,$1 ,$3)]
         [(exp > exp) `(> ,$1 ,$3)]
         [(exp >= exp) `(>= ,$1 ,$3)]
         [(exp <= exp) `(<= ,$1 ,$3)]
         [(exp + exp) `(+ ,$1 ,$3)]
         [(exp - exp) `(- ,$1 ,$3)]
         [(exp * exp) `(* ,$1 ,$3)]
         [(exp / exp) `(quotient ,$1 ,$3)]
         [(exp % exp) `(modulo ,$1 ,$3)]
         [(OP exp CP) $2])
    )
  
   )
  )


 ; procedure calls the parser passing it a lambda (that calls the lexer providing it with the input) with which it will use to get values from the lexer, as many valuse at a time as it sees fit.
; i.e. according to what the parsing rules perscribe

;idea of how to break the two up, this way, taken from Sandre Sjursen's code

(define (calceval ip)
  (calcp (lambda () (calcl ip))))

; calls the evaluator  to get back results
(define calcres (lambda (in) (calceval (open-input-file in))))

    


; procedure that assures, that in case of a variable, its underlying value, stored in memory, is returned.
(define (assureNum val)
  (cond
    [(number? val) val]
    [else (hash-ref vars  val (lambda ()  "used unassigned var"))]))

; procedure that evaluates expressions or just returns the value in case where no evaluation need to be done
(define (evalExp exp)
   
  (letrec ((evalExp
            
            (lambda ( Op  left right)
             
              (cond
              
                [ (and (not (list? left)) (not (list? right))) ; neither sides are lists
                  ((eval Op ns) (assureNum left) (assureNum right))  ]
                [(and (not(list? left)) (list? right));only right is a list
                 ((eval Op ns) (assureNum left) (evalExp (first right) (second right) (third right)))]
                [(not(list? right)); only left is list
                 ((eval Op ns)  (evalExp (first left) (second left) (third left)) (assureNum right))]
                [else; both, left and right are lists
                 ( (eval Op ns) (evalExp (first left) (second left) (third left)) (evalExp (first right) (second right) (third right)) )]
                )
              )
            )); case where there's no list. i.e. just a number or variable
    (if (not (list? exp))  (assureNum exp )   (evalExp    (first exp)   (second exp) (third exp)))     
    )
  )

 ; procedure simply traverses through the tree, unwrapping each branch as it goes along until it gets to the last which will be the empty list.
(define (extractState lst)
  (if   (empty? lst)  lst  
        (begin
  
          (cond
            [ (equal? 'assign (car(first lst)))   (hash-set! vars (cadr(first lst)) (evalExp (first (cddr(first lst)))))]; case assign statement
                                       
            [(equal? 'print (car(first lst)))  (displayln (string-join  (map number->string (map assureNum (flatten (cadr (first lst))))) ", " ))]; modified from Sandre Sjursen
            [(equal? 'if (car(first lst))) (if (= (length (first lst)) 3) (let ((res  (evalExp (cadr(first lst))))) (if (or (equal? res 1)(equal? res #t)) (extractState (cddr(first lst))) (or (or (equal? res 0)(equal? res #f))  (error "if condition may only evaluate to 0/true or 1/false"))));case one branch if
                                               (let  ((res (evalExp (cadr(first lst))))) (if (or (equal? res 1)(equal? res #t))   (extractState (list (caddr(first lst)))) (or (and  (or (equal? res 0)(equal? res #f))   (extractState   (cdddr(first lst))))   ( error "if may only evaluate to 0/true or 1/false")))); case two branch if
                                               )] 
            [(equal? 'while (car(first lst)))  (let ((res (evalExp (cadr(first lst))) )) (if (or (equal? res 1) (equal? res #t)) (begin (extractState  (caddr(first lst)))  (extractState (list (first lst))))   (or (or (equal? res 0) (equal? res #f)) (error "while condition may only evaluate to 0/true or 1/false"))))
                                               ]
        
      
            )
          (unless (equal? (rest lst) '())  (extractState (first (rest lst)))); recurses to do the rest of the statements in the prog
          )
        )
  )
 


(define (interpret fileName)
  (extractState  (calcres fileName)))

  
              
 













