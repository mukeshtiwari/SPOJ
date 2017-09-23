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
     [(exp Plus exp) (arith-exp '+ $1 $3)]
     [(exp Minus exp) (arith-exp '- $1 $3)]
     [(exp Mult exp) (arith-exp '* $1 $3)]
     [(exp Div exp) (arith-exp '/ $1 $3)]))))




(define ab-test-in (open-input-string "((x))"))

(define (lex-this lexer input) (lambda () (lexer input)))

(let ((input (open-input-string "(a+(b*c))")))
  (expression-parser (lex-this expression-lexer input)))
