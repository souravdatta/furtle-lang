#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)


(define-tokens FurtleTok [SYMBOL NUMBER OP])
(define-empty-tokens FurtleTok* [TO END EOF NEWLINE REPEAT IF ELSE WHEN THEN DO])

(define furtle-lexer (lexer
                      [(eof) (token-EOF)]
                      ;;[#\newline (token-NEWLINE)]
                      [(:or whitespace blank iso-control) (furtle-lexer input-port)]
                      [(:or (:: #\t #\o)
                            (:: #\T #\O)) (token-TO)]
                      [(:or (:: #\d #\o)
                            (:: #\D #\O)) (token-DO)]
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
                      [(:or (:: #\t #\h #\e #\n)
                            (:: #\T #\H #\E #\N)) (token-THEN)]
                      [(:or #\+ #\- #\* #\/ #\< #\> #\= (:: #\& #\&) (:: #\| #\|) (:: #\! #\=) (:: #\: #\=) #\( #\)) (token-OP lexeme)]
                      [(:: alphabetic (:* (:or alphabetic numeric))) (token-SYMBOL (string->symbol lexeme))]
                      [(:+ numeric) (token-NUMBER (string->number lexeme))]))


(define (string->tokens s)
  (let ([ip (open-input-string s)])
    (let looper ([tokens '()])
      (let ([next-token (furtle-lexer ip)])
        (cond
          ((eq? next-token 'EOF) (reverse tokens))
          (else (looper (cons next-token tokens))))))))


(define stack '())

(define (push-op v)
  (set! stack (cons v stack)))

(define (pop-op)
  (if (empty? stack)
      'eof
      (let ([top (car stack)])
        (set! stack (cdr stack))
        top)))

(define furtle-parser (parser
                       (tokens FurtleTok FurtleTok*)
                       (start funcalls)
                       (end EOF)
                       (error (λ (tok tname tval)
                                (displayln (format "tok_~a, tok_val_~a" tname tval))))
                       (grammar
                        (funcalls ((funcall) (void))
                                  ((funcall funcalls) (void)))
                        (funcall ((SYMBOL arglist) (push-op (list 'funcall $1))))
                        (arglist
                         (() (push-op (list 'arg '())))
                         ((NUMBER arglist) (push-op (list 'arg $1))))))) 
                       

(define (parse-string s)
  (let ([is (open-input-string s)])
    (set! stack '())
    (furtle-parser (λ () (furtle-lexer is)))
    (displayln (format "~a" (reverse stack)))))

