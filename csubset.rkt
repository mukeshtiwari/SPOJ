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
   [(:+ (char-range #\0 #\9)) (token-Integer lexeme)]
   [(:: "\"" (:* (char-complement "\"")) "\"") (token-String lexeme)]
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (expression-lexer input-port)] 
   [whitespace (expression-lexer input-port)]
   [(eof) (token-Eof)]))


; Working for every all test case http://rosettacode.org/wiki/Compiler/lexical_analyzer
; I haven't considered 
(define simp-prog (open-input-string "
/*
  All lexical tokens - not syntactically correct, but that will
  have to wait until syntax analysis
 */
/* Print   */  print    /* Sub     */  -
/* Putc    */  putc     /* Lss     */  <
/* If      */  if       /* Gtr     */  >
/* Else    */  else     /* Leq     */  <=
/* While   */  while    /* Geq     */  >=
/* Lbrace  */  {        /* Eq      */  ==
/* Rbrace  */  }        /* Neq     */  !=
/* Lparen  */  (        /* And     */  &&
/* Rparen  */  )        /* Or      */  ||
/* Uminus  */  -        /* Semi    */  ;
/* Not     */  !        /* Comma   */  ,
/* Mul     */  *        /* Assign  */  =
/* Div     */  /        /* Integer */  42
/* Mod     */  %        /* String  */  \"String literal\"
/* Add     */  +        /* Ident   */  variable_name"))

(expression-lexer simp-prog)
