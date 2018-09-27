#lang rosette

(require "../Lib/symbolic-manipulation.rkt")

(provide interpret)

(define (interpret-literal! lit-list)
  (match (list-ref lit-list 0)
    ;; The third in the list is the actual val
    ['Int (list-ref lit-list 2)]))

(define (interpret-Name! name-list)
  (match (list-ref name-list 0)
    ['Symbol (match (list-ref name-list 2)
	       ["+" +]
	       )]))
		 

;; Interprets a qualified name
(define (interpret-QName! qname-list)
  (match (list-ref qname-list 0)
    ['UnQual (interpret-Name! (list-ref qname-list 2))]))

;; Interprets a qualified operation
(define (interpret-QOp! qop-list)
  (match (list-ref qop-list 0)
    ['QVarOp (interpret-QName! (list-ref qop-list 2))]))

;; Interprets a given expression.
(define (interpret-exp! exp s-expr-port)
  (match exp 
    ;; If the expression is a list, then convert it to an input
    ;; and interpret said input
    ['Lit (begin
	    ;; Discard SrcSpan
	    (read s-expr-port)
	    ;; Create input for literal
	    (interpret-literal! (read s-expr-port)))]
    ;; Infix operator application; read arguments, operator, apply
    ['InfixApp (begin
		 (read s-expr-port)
		 (define item-1 (interpret (open-input-list (read s-expr-port))))
		 (define op (interpret-QOp! (read s-expr-port)))
		 (define item-2 (interpret (open-input-list (read s-expr-port))))
		 (op item-1 item-2))]))

(define (interpret s-expr-port)
  (define s-expr (read s-expr-port))
  (cond
   ;; If Symbol, then we are at a top level of an s-expr,
   ;; interpret it!
   [(symbol? s-expr) (interpret-exp! s-expr s-expr-port)]
   ;; If EOF is reached, return a string for Done
   [(eof-object? s-expr) "Done!"]
   ;; else continue to interpret the expression
   [else (interpret s-expr-port)]))
