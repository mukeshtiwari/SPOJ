#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)


(define-tokens a (Var))
(define-empty-tokens b (Plus Minus Mult Div LPar RPar Eof))

(define expression-lexer 
  (lexer
   ["+" (token-Plus)]
   ["-" (token-Minus)]
   ["*" (token-Mult)]
   ["/" (token-Div)]
   ["(" (token-LPar)]
   [")" (token-RPar)]
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) (token-Var lexeme)]
   [whitespace (expression-lexer input-port)]
   [(eof) (token-Eof)]))


(define-struct var-exp (i) #:transparent)
(define-struct arith-exp (op expl expr) #:transparent)

(define expression-parser
  (parser
   (start exp)
   (end Eof)
   (error void)
   (tokens a b)
   (precs (left Plus Minus)
          (left Mult Div))
   (grammar
    (exp
     [(Var) (var-exp $1)]
     [(LPar exp RPar) $2]
     [(exp Plus exp) (arith-exp "+" $1 $3)]
     [(exp Minus exp) (arith-exp "-" $1 $3)]
     [(exp Mult exp) (arith-exp "*" $1 $3)]
     [(exp Div exp) (arith-exp "/" $1 $3)]))))

(define (string-expression ast)
  (match ast
    [(var-exp i) i]
    [(arith-exp "+" expl expr)
     (string-append 
      (string-expression expl)
      "+"
      (string-expression expr))]
    [(arith-exp "-" expl expr)
     (let ([fexp (string-expression expl)]
           [sexp (string-expression expr)])
       (match expr
         [(var-exp i) (string-append fexp "-" sexp)]
         [(arith-exp op l r)
          (match op
            [(or "+" "-") (string-append fexp "-" "(" sexp ")")]
            [else (string-append fexp "-" sexp)])]))]
    [(arith-exp "*" expl expr)
     (let ([fexp (string-expression expl)]
           [sexp (string-expression expr)])
       (match (list expl expr)
         [(list (var-exp i) (var-exp j)) (string-append fexp "*" sexp)]
         [(list (arith-exp op l r) (var-exp j))
          (match op
            [(or "+" "-") (string-append "(" fexp ")" "*" sexp)]
            [else (string-append fexp "*" sexp)])]
         [(list (var-exp i) (arith-exp op l r))
          (match op
            [(or "+" "-") (string-append fexp "*" "(" sexp ")")]
            [else (string-append fexp "*" sexp)])]
         [(list (arith-exp opl cexpl cexpr) (arith-exp opr dexpl dexpr))
          (match (list opl opr)
            [(or (list "+" "+")
                 (list "+" "-")
                 (list "-" "+")
                 (list "-" "-")) (string-append "(" fexp ")" "*" "(" sexp ")")]
            [(or (list "+" t) (list "-" t))
             (string-append "(" fexp ")" "*" sexp)]
            [(or (list t "+") (list t "-"))
             (string-append fexp "*" "(" sexp ")")]
            [else (string-append fexp "*" sexp)])]))]
    [(arith-exp "/" expl expr)
     (let ([fexp (string-expression expl)]
           [sexp (string-expression expr)])
       (match (list expl expr)
         [(list (var-exp i) (var-exp j)) (string-append fexp "/" sexp)]
         [(list (arith-exp op l r) (var-exp j))
          (match op
            [(or "+" "-") (string-append "(" fexp ")" "/" sexp)]
            [else (string-append fexp "/" sexp)])]
         [(list (var-exp i) (arith-exp op l r)) (string-append fexp "/" "(" sexp ")")]
         [(list (arith-exp opl cexprl cexpr) (arith-exp opr dexpl dexpr))
          (match opl
            [(or "+" "-") (string-append "(" fexp ")" "/" "(" sexp ")")]
            [else (string-append fexp "/" "(" sexp ")")])]))]))
          
          
(define (lex-this lexer input) (lambda () (lexer input)))

(define (solve-string str)
  (let ((input (open-input-string str)))
    (string-expression (expression-parser (lex-this expression-lexer input)))))



(let loop ()
  (define a (string->number (read-line (current-input-port) 'any)))
  (let/ec break
    (define (repeat cnt)
      (cond
        [(>= cnt a) (break)]
        [else
         (define b (read-line (current-input-port) 'any))
         (fprintf (current-output-port) "~a\n"
                  (solve-string b))
         (repeat (+ 1 cnt))]))
    (repeat 0)))