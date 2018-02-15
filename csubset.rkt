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
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) (token-Identifier lexeme)]
   [(:+ (char-range #\0 #\9)) (token-Integer lexeme)]
   [whitespace (expression-lexer input-port)]
   [(eof) (token-Eof)]))

(define simp-prog (open-input-string "2 + 3 ; print(\"helllo\")"))
(expression-lexer simp-prog)
