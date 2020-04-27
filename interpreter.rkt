 
#lang racket
 (define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(require loop)
;; An interactive calculator inspired by the calculator example in the bison manual.


;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR  ))
(define-empty-tokens op-tokens ( newline  = OC CC DEL OP CP + - * / || %   or && == != >= <= > <  EOF PRINT WHILE IF ELSE  ))
 
;(define || (lambda (a b) (or a b)))
 (define vars (make-hash))

(define-lex-abbrevs
 (lower-letter (:/ "a" "z"))

 (upper-letter (:/ #\A #\Z))
 (digit (:/ "0" "9")))
 
(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space  #\return #\newline  ) (calcl input-port)]
   ;; (token-newline) returns 'newline
   ;   [  #\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
       [ (:= 2  #\|)   (token-||)];(string->symbol lexeme)];
   [(:or "=" "+" "-" "*" "/" "%" "&&"      "==" "!=" ">=" "<=" ">" "<") (string->symbol lexeme)]
  
   ["(" 'OP]
   [")" 'CP]
    ["{" 'OC]
   ["}" 'CC]
    [ "print" 'PRINT   ]
   [#\,  'DEL  ]
    [ "while" 'WHILE ]
   [ "if" 'IF ]
    [ "else" 'ELSE ] ; check to make sure these aren't used as variables
    ;  [(:or "print" #\, "while" "if" "else") (string->symbol lexeme)]

   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   ))
   
(define (string->char s)  (car (string->list s)))

(define calcp
  (parser
   (start   start)
   (end   EOF)
   (tokens value-tokens op-tokens  )
  (error (λ(ok? name value) (if (boolean? value) (printf "Couldn't parse: ~a\n" name) (printf "Couldn't parse: ~a\n" value)))) ; fix so that it prints  value or name at appropriate times
   (precs 
    ; (right PRINT)
    (left DEL); change to left maybe as in tutorialpoint?


    (right =) ;from lowest to highest
   ;  (right ELSE)
          ;   (right IF) ; causing shift reduce confilicts
            
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
           [() '()]
             
         ;  [(error start) $2]  
          
          
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






 
(define (calceval ip)
              (calcp (lambda () (calcl ip))))

 (define calcres (lambda (in) (calceval (open-input-file in))))

    



(define (assureNum val)
  (cond
    [(number? val) val]
    [else (hash-ref vars  val (lambda ()  "used unassigned var"))]))


(define (evalExp exp)
   
  (letrec ((evalExp
            
            (lambda ( Op  left right)
             
            (cond
              
              [ (and (not (list? left)) (not (list? right)))
                  ( (eval Op ns) (assureNum left) (assureNum right))  ];(printf "~a\n" "left and right arne't lists")];(printf "~a\n" (and (not (list? left)) (list? right)))]
               [(and (not(list? left)) (list? right))
                ((eval Op ns) (assureNum left) (evalExp (first right) (second right) (third right)))];(printf "~a\n" "only right is a list")];(printf "~a\n" (and (not (list? left)) (list? right)))];  
               [(not(list? right))
                ((eval Op ns)  (evalExp (first left) (second left) (third left)) (assureNum right))]; (printf "~a\n" "only left is a list")];
              [else
               ( (eval Op ns) (evalExp (first left) (second left) (third left)) (evalExp (first right) (second right) (third right)) )];(printf "~a\n" "both are lists")];if  (= (length exp) 1) (assureNum exp) 
              )
            )
          ))
       (if (not (list? exp))  (assureNum exp )   (evalExp    (first exp)   (second exp) (third exp)))     
  )
)

    
 (define (extractState lst)
   (if   (empty? lst)  lst  
   (begin
  
     (cond
       [ (equal? 'assign (car(first lst)))   (hash-set! vars (cadr(first lst)) (evalExp (first (cddr(first lst))))) ]
                                       
       [ (equal? 'print (car(first lst)))  (displayln (string-join  (map number->string (map assureNum (flatten (cadr (first lst))))) ", " ))] 
       [(equal? 'if (car(first lst))) (if (= (length (first lst)) 3) (let ((res  (evalExp (cadr(first lst))))) (if (or (equal? res 1)(equal? res #t)) (extractState (cddr(first lst))) (or (or (equal? res 0)(equal? res #f))  (error "if condition may only evaluate to 0/true or 1/false"))))
                                                                     (let  ((res (evalExp (cadr(first lst))))) (if (or (equal? res 1)(equal? res #t))   (extractState (list (caddr(first lst)))) (or (and  (or (equal? res 0)(equal? res #f))   (extractState   (cdddr(first lst))))   ( error "if may only evaluate to 0/true or 1/false"))))
                                          )] 
      [(equal? 'while (car(first lst)))  (let ((res (evalExp (cadr(first lst))) )) (if (or (equal? res 1) (equal? res #t)) (begin (extractState  (caddr(first lst)))  (extractState (list (first lst))))   (or (or (equal? res 0) (equal? res #f)) (error "while condition may only evaluate to 0/true or 1/false"))))
                                             ]
        
      
    )
    (unless (equal? (rest lst) '())  (extractState (first (rest lst))))
   )
)
   )

   (extractState  (calcres "cmmExamples/example1.cmm"))
 (extractState  (calcres "cmmExamples/example2.cmm"))
 (extractState  (calcres "cmmExamples/example3.cmm"))
 (extractState  (calcres "cmmExamples/example4.cmm"))
 (extractState  (calcres "cmmExamples/example5.cmm"))

 
              
         
 
 
 













