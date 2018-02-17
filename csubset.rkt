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
(define-struct print-exp (expr) #:transparent)
(define-struct put-exp (expr) #:transparent)
(define-struct logic-exp (op expl expr) #:transparent)
(define-struct unary-exp (op expr) #:transparent)


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
     [(stmt) $1])
    (stmt
     [(Semi) '()]
     [(Identifier Assign exp Semi) (ass-exp $1 $3)]
     [(While pexp stmt) (while-exp $2 $3)]
     [(If pexp stmt Else stmt) (if-else-exp $2 $3 $5)]
     [(If pexp stmt) (if-exp $2 $3)]
     [(Print Lpar prt-list Rpar Semi) (print-exp $3)]
     [(Putc pexp Semi) (put-exp $2)]
     [(Lbr stmt-list Rbr) $2])
    (prt-list
     [(String) (str-exp $1)]
     [(exp Comma String) (append $1 $3)]
     [(exp Comma exp) (append $1 $3)]
     [(exp) $1])  
    (pexp
     [(Lpar exp Rpar) $2])
    (exp
     [(and-exp Or and-exp) (logic-exp 'Or $1 $3)]
     [(and-exp) $1])
    (and-exp
     [(eq-exp And eq-exp) (logic-exp 'And $1 $3)]
     [(eq-exp) $1])
    (eq-exp
     [(rel-exp Eq rel-exp) (logic-exp 'Eq $1 $3)]
     [(rel-exp Neq rel-exp) (logic-exp 'Neq $1 $3)])
    (rel-exp
     [(add-exp Le add-exp) (logic-exp 'Lt $1 $3)]
     [(add-exp Leq add-exp) (logic-exp 'Leq $1 $3)]
     [(add-exp Gt add-exp) (logic-exp 'Gt $1 $3)]
     [(add-exp Geq add-exp) (logic-exp 'Geq $1 $3)])
    (add-exp
     [(mult-exp Add mult-exp) (arith-exp 'Add $1 $3)]
     [(mult-exp Sub mult-exp) (arith-exp 'Sub $1 $3)]
     [(mult-exp) $1])
    (mult-exp
     [(primary Mult primary) (arith-exp 'Mult $1 $3)]
     [(primary Div primary) (arith-exp 'Div $1 $3)]
     [(primary Mod primary) (arith-exp 'Mod $1 $3)]
     [(primary) $1])
    (primary
     [(Identifier) (ident-exp $1)]
     [(Integer) (int-exp $1)]
     [(Lpar exp Rpar) $2]
     [(Add primary) $2]
     [(Sub primary) (unary-exp 'Sub $2)]
     [(Not primary) (unary-exp 'Neg $2)]))))
     
     
    

(define simp-prog (open-input-string "if (x == 1) average = total / num_items;"))

(expression-lexer simp-prog)

(define (lex-this lexer input) (lambda () (lexer input)))


(define (solve-string str)
  (let ((input (open-input-string str)))
    (expression-parser (lex-this expression-lexer input))))
