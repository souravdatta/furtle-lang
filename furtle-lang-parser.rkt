#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)


(define *current-stack* (make-parameter '()))

(define *current-vars* (make-parameter (make-hasheq)))

(define vars (make-hasheq))
(define tvars (make-hasheq))
(define fvars (make-hasheq))

(define (reset-temp-bindings!)
  (hash-clear! tvars)
  (hash-clear! fvars))

(define (reset-vars! vrs)
  (hash-clear! vrs))

(define (bind-var! var val)
  (let ([the-hash (cond
                    ((eq? if-flag 'true-part) tvars)
                    ((eq? if-flag 'false-part) fvars)
                    (else (*current-vars*)))])
    (let ([pvalue (hash-ref the-hash var (lambda () #f))])
      (if pvalue
          (error (format "Cannot rebind ~a, already bound to ~a"
                         var
                         pvalue))
          (hash-set! the-hash var val)))))

(define (merge-bindings! result-val)
  (set if-flag #f)
  (hash-map (if result-val tvars fvars)
            (λ (k v)
              (bind-var! k v))))
     
(define (recall-var var)
  (hash-ref (*current-vars*) var (λ () (error (format "Unable to get bindings for ~a" var)))))

(define stack '())
(define tstack '())
(define fstack '())

(define (reset-stack!)
  (set! stack '()))

(define (reset-temp-stacks!)
  (set! tstack '())
  (set! fstack '()))

(define if-flag #f)

(define (reset-if-flag!)
  (set! if-flag #f))

(define (set-if-flag! v)
  (set! if-flag v))

(define (push-op! v)
  (cond
    ((eq? if-flag 'true-part) (set! tstack (cons v tstack)))
    ((eq? if-flag 'false-part) (set! fstack (cons v fstack)))
    (else (set! stack (cons v stack)))))

(define (merge-temp-stack! result-val)
  (if result-val
      (set! stack (append tstack stack))
      (set! stack (append fstack stack))))

(define (pop-op!)
  (let ([stk (*current-stack*)])
    (if (empty? stk)
        'eof
        (let ([top (car stk)])
          (set! stk (cdr stk))
          top))))

(define (simplify-exp lhs op rhs)
  (list op
        (simplify lhs)
        (simplify rhs)))

(define (simplify exp)
  (cond
    ((number? exp) exp)
    ((boolean? exp) exp)
    ((symbol? exp) (recall-var exp))
    ((list? exp) exp)
    (else (simplify-exp (car exp) (cadr exp) (caddr exp)))))

(define (our-not-equal-numeric? . params)
  (not (apply = params)))

(define (eval-simplified expr)
  (let ([nm (make-base-namespace)])
    (namespace-set-variable-value! '!=
                                   our-not-equal-numeric?
                                   #f
                                   nm)
    (eval expr nm)))

(define-tokens FurtleTok [SYMBOL NUMBER OP_LOGICAL OP_ARITHMATIC])
(define-empty-tokens FurtleTok* [TO END EOF SEP REPEAT
                                    IF ELSE WHEN THEN TRUE FALSE
                                    DO OP_ASSIGN OP_OPEN_PAREN OP_CLOSE_PAREN])

(define furtle-lexer (lexer
                      [(eof) (token-EOF)]
                      ;;[#\newline (token-NEWLINE)]
                      [#\; (token-SEP)]
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
                      [(:or (:: #\t #\r #\u #\e)
                            (:: #\T #\R #\U #\E)) (token-TRUE)]
                      [(:or (:: #\f #\a #\l #\s #\e)
                            (:: #\F #\A #\L #\S #\E)) (token-FALSE)]
                      [(:: #\: #\=) (token-OP_ASSIGN)]
                      [#\( (token-OP_OPEN_PAREN)]
                      [#\) (token-OP_CLOSE_PAREN)]
                      [(:or #\+ #\- #\* #\/) (token-OP_ARITHMATIC (string->symbol lexeme))]
                      [(:or #\< #\> #\= (:: #\a #\n #\d) (:: #\o #\r) (:: #\n #\o #\t)
                            (:: #\! #\=))
                       (token-OP_LOGICAL (string->symbol lexeme))]
                      [(:: alphabetic (:* (:or alphabetic numeric))) (token-SYMBOL (string->symbol lexeme))]
                      [(:+ numeric) (token-NUMBER (string->number lexeme))]))


(define (string->tokens s)
  (let ([ip (open-input-string s)])
    (let looper ([tokens '()])
      (let ([next-token (furtle-lexer ip)])
        (cond
          ((eq? next-token 'EOF) (reverse tokens))
          (else (looper (cons next-token tokens))))))))


(define furtle-parser
  (parser
   (tokens FurtleTok FurtleTok*)
   (start statements)
   (end EOF)
   (error (λ (tok tname tval)
            (displayln (format "Error in parsing at '~a', token ~a" tval tname))))
   (grammar
    (statement ((assignment) (void))
               ((if-exp) (void))
               ((funcall) (void)))
    (statements ((statement SEP) (void))
                ((statement SEP statements) (void)))
    (assignment ((SYMBOL OP_ASSIGN rvalue)
                 (bind-var! $1 $3)))
    (if-exp ((if-op rvalue then-op statements else-op statements end-op)
             (begin
               (merge-temp-stack! $2)
               (merge-bindings! $2))))
    (if-op ((IF) (begin (reset-temp-stacks!)
                        (reset-temp-bindings!)
                        (reset-if-flag!))))
    (then-op ((THEN) (set-if-flag! 'true-part)))
    (else-op ((ELSE) (set-if-flag! 'false-part)))
    (end-op ((END) (reset-if-flag!)))
    (rvalue ((SYMBOL) (recall-var $1))
            ((NUMBER) $1)
            ((TRUE) #t)
            ((FALSE) #f)
            ((OP_OPEN_PAREN numeric-exp OP_CLOSE_PAREN) $2)
            ((OP_OPEN_PAREN logical-exp OP_CLOSE_PAREN) $2))
    (numeric-exp ((NUMBER) $1)
                 ((rvalue OP_ARITHMATIC rvalue) (eval-simplified
                                                 (simplify-exp $1 $2 $3))))
    (logical-exp ((TRUE) #t)
                 ((FALSE) #f)
                 ((rvalue OP_LOGICAL rvalue) (eval-simplified
                                              (simplify-exp $1 $2 $3))))
    (funcall ((SYMBOL arglist) (push-op! $1)))
    (arglist
     (() (void))
     ((rvalue arglist) (push-op! $1))))))
                       

(define (parse-string s)
  (let ([is (open-input-string s)])
    (reset-stack!)
    (reset-temp-stacks!)
    (reset-if-flag!)
    (reset-vars! vars)
    (parameterize ([*current-vars* vars])
      (furtle-parser (λ () (furtle-lexer is)))
    (displayln (format "stack: ~a" (reverse stack)))
    (displayln (format "vars: ~a" vars)))))
 
