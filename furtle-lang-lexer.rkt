#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))


(define-tokens TurtleTok [SYMBOL NUMBER OP])
(define-empty-tokens TurtleTok* [TO END EOF NEWLINE REPEAT IF ELSE WHEN])

(define turtle-lexer (lexer
                      [(eof) (token-EOF)]
                      ;;[#\newline (token-NEWLINE)]
                      [(:or whitespace blank iso-control) (turtle-lexer input-port)]
                      [(:or (:: #\t #\o)
                            (:: #\T #\O)) (token-TO)]
                      [(:or (:: #\e #\n #\d)
                            (:: #\E #\N #\D)) (token-END)]
                      [(:or (:: #\r #\e #\p #\e #\a #\t)
                            (:: #\R #\E #\P #\E #\A #\T)) (token-REPEAT)]
                      [(:or (:: #\i #\f)
                            (:: #\I #\F)) (token-IF)]
                      [(:or (:: #\e #\l #\s #\e)
                            (:: #\E #\L #\S #\E)) (token-ELSE)]
                      [(:or (:: #\w #\h #\e #\n)
                            (:: #\W #\H #\E #\N)) (token-WHEN)]
                      [(:or #\+ #\- #\* #\/ #\< #\> #\= (:: #\& #\&) (:: #\| #\|) (:: #\! #\=) (:: #\: #\=) #\( #\)) (token-OP lexeme)]
                      [(:: alphabetic (:* (:or alphabetic numeric))) (token-SYMBOL (string->symbol lexeme))]
                      [(:+ numeric) (token-NUMBER (string->number lexeme))]))


;; helper test function
(define (string->tokens s)
  (let ([ip (open-input-string s)])
    (let looper ([tokens '()])
      (let ([next-token (turtle-lexer ip)])
        (cond
         ((eq? next-token 'EOF) (reverse tokens))
         (else (looper (cons next-token tokens))))))))
 
