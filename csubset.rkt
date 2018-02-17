#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)


;; Lexical analyzer of subset of C programming language
(define-tokens a (Identifier Integer String))
(define-empty-tokens b (Add Sub Mult Div Mod Le Leq Gt Geq Eq Neq Not Assign And Or
                            Lpar Rpar Lbr Rbr Semi Comma If Else While Print Putc Eof))
                         


(define expression-lexer 
  (lexer
   ["if" (token-If)]
   ["else" (token-Else)]
   ["while" (token-While)]
   ["print" (token-Print)]
   ["putc" (token-Putc)]
   ["+" (token-Add)]
   ["-" (token-Sub)]
   ["*" (token-Mult)]
   ["/" (token-Div)]
   ["%" (token-Mod)]
   ["<" (token-Le)]
   ["<=" (token-Leq)]
   [">" (token-Gt)]
   [">=" (token-Geq)]
   ["==" (token-Eq)]
   ["!=" (token-Neq)]
   ["!" (token-Not)]
   ["=" (token-Assign)]
   ["&&" (token-And)]
   ["||" (token-Or)]
   ["(" (token-Lpar)]
   [")" (token-Rpar)]
   ["{" (token-Lbr)]
   ["}" (token-Rbr)]
   [";" (token-Semi)]
   ["," (token-Comma)]
   [(concatenation
     (:or (char-range #\a #\z) (char-range #\A #\Z) #\_)
     (:* (:or (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9) #\_))) (token-Identifier lexeme)]
   [(:+ (char-range #\0 #\9)) (token-Integer (string->number lexeme))]
   [(:: "\"" (:* (char-complement "\"")) "\"") (token-String lexeme)]
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (expression-lexer input-port)] 
   [whitespace (expression-lexer input-port)]
   [(eof) (token-Eof)]))


; Working for every all test case http://rosettacode.org/wiki/Compiler/lexical_analyzer
; I haven't considered some of the corner cases. 




;; Writing parser. After finishing move it to separate file

(define-struct prog-exp (expl expr) #:transparent)
(define-struct if-else-exp (expt expl expr) #:transparent)
(define-struct if-exp (expl expr) #:transparent)
(define-struct seq-exp (expl expr) #:transparent)
(define-struct while-exp (expl expr) #:transparent)
(define-struct ass-exp (expl expr) #:transparent)
(define-struct ident-exp (i) #:transparent)
(define-struct int-exp (i) #:transparent)
(define-struct str-exp (i) #:transparent)
(define-struct arith-exp (op expl expr) #:transparent)


(define expression-parser
  (parser
   (start stmt-list)
   (end Eof)
   (error void)
   (tokens a b)
   (precs (left Add Sub)
          (left Mult Div))
   (grammar
    (stmt-list
     [(stmt) $1]
     [() '()])
    (stmt
     [(Semi) '()]
     [(Identifier Assign exp Semi) (ass-exp $1 $3)]
     [(While exp stmt) (while-exp $2 $3)]
     [(If exp stmt Else stmt) (if-else-exp $2 $3 $5)]
     [(If exp stmt) (if-exp $2 $3)]
     [(Lbr stmt-list Rbr) $2])
   
    
    (exp
     [(Identifier) (ident-exp $1)]
     [(Integer) (int-exp $1)]
     [(String) (str-exp $1)]
     [(Lpar exp Rpar) $2]
     [(exp Add exp) (arith-exp "+" $1 $3)]
     [(exp Sub exp) (arith-exp "-" $1 $3)]
     [(exp Mult exp) (arith-exp "*" $1 $3)]
     [(exp Div exp) (arith-exp "/" $1 $3)]))))


(define simp-prog (open-input-string "x = 1;"))

(expression-lexer simp-prog)

(define (lex-this lexer input) (lambda () (lexer input)))


(define (solve-string str)
  (let ((input (open-input-string str)))
    (expression-parser (lex-this expression-lexer input))))
