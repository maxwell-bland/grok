#lang rosette

(require "../Lib/symbolic-manipulation.rkt")
(require "./symbol-table.rkt")

(provide interpret)

(define (interpret-literal! lit-list)
  (match (car lit-list)
    ;; The third in the list is the actual val
    ['Int (list-ref lit-list 2)]))

(define (interpret-Name! name-list)
  (match (car name-list)
    ['Symbol (match (list-ref name-list 2)
	       ["+" +]
	       )]
    ;; Match a varid or conid; TODO: conid
    ['Ident (create/get-ident (list-ref name-list 2))]))

;; Interprets a qualified name
(define (interpret-QName! qname-list)
  (match (car qname-list)
    ['UnQual (interpret-Name! (list-ref qname-list 2))]))

;; Interprets a qualified operation
(define (interpret-QOp! qop-list)
  (match (car qop-list)
    ['QVarOp (interpret-QName! (list-ref qop-list 2))]))

;; Interprets a given expression.
(define (interpret-exp! exp-list)
  (match (car exp-list) 
    ['Lit (interpret-literal! (list-ref exp-list 2))]
    ['Var (interpret-QName! (list-ref exp-list 2))]
    ['If (cons 'If (map interpret-exp!
			(list (list-ref exp-list 2)
			      (list-ref exp-list 3)
			      (list-ref exp-list 4))))]
    ;; TODO make this actually construct vals
    ['Con (interpret-QName! (list-ref exp-list 2))]
    ;; Infix operator application; read arguments, operator, apply
    ['InfixApp (let ([item-1 (interpret-exp! (list-ref exp-list 2))]
		     [op (interpret-QOp! (list-ref exp-list 3))]
		     [item-2 (interpret-exp! (list-ref exp-list 4))])
		 (apply op (list item-1 item-2)))]))

;; Arguements to a function, but really any pattern to match
(define (interpret-Pat! pat-list)
  (match (car pat-list)
    ['PVar (interpret-Name! (list-ref pat-list 2))]))

;; The right hand side of a function binding, pattern binding, or
;; a case alternative. TODO: gaurded rhs.
(define (interpret-Rhs! rhs-list)
  (match (car rhs-list)
    ['UnGuardedRhs (interpret-exp! (list-ref rhs-list 2))]))

;; Clauses for function binding
;; todo: make this work for the other clause
(define (interpret-Match! match-list)
  (match (car match-list)
    ;; Todo, make this handle mre than one argument
    ;; Todo: where bindings
    ['Match (let ([fun-id (interpret-Name! (list-ref match-list 2))]
		  [args (interpret-Pat! (list-ref match-list 3))]
		  [rhs (interpret-Rhs! (list-ref match-list 4))])
	      (list 'Fun fun-id args rhs))]))

(define (interpret-decl! decl-list)
  (match (car decl-list)
    ;; TODO: make this work for more than one match
    ['FunBind (interpret-Match! (list-ref decl-list 2))]))

;; Takes an input port representing an s-expr and builds it into a list by
;; reding each part
(define (build-list-exp s-expr-port) 
  (let ([s-expr (read s-expr-port)])
    (cond
     [(eof-object? s-expr) '()]
     [else (cons s-expr (build-list-exp s-expr-port))])))

(define (symbolic-exec decl top-level)
  (cond 
    ;; TODO: recursion
    [(and (list? decl) (eq? 'Fun (car decl))) (symbolic-exec (list-ref decl 3) top-level)]
    [(and (list? decl) (eq? 'If (car decl))) (map (lambda (b) (and (symbolic-exec (list-ref decl 1) (void)) b))
	      (list
	       (symbolic-exec (list-ref decl 2) top-level)
	       (symbolic-exec (list-ref decl 3) top-level)))]
    [else (cond 
	    [(void? top-level) decl]
	    [else (apply = (list top-level decl))])]))

(define (interpret s-expr-port interpret-type)
  (match interpret-type
    ["expr" (interpret-exp! (build-list-exp s-expr-port))]
    ;; First interpret the declaration
    [else (begin
	    (define-symbolic top-level integer?)
	    (letrec ([decl (interpret-decl! (build-list-exp s-expr-port))]
		     ;; OR all the paths together
		     [paths (let ([constraints (symbolic-exec decl top-level)])
			      (cond
			       [(list? constraints) (apply || constraints)]
			       [else constraints])
			     )])
	      (begin
		(solve (assert (and paths (> top-level 10))))
		)
	      ;; Finally, assert the constraint and the paths
	      ))]))
