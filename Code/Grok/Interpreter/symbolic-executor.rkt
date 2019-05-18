#lang rosette

(provide symbolic-exec)

;; Hash of the bodies of functions
(define fun-bodies (make-hash))
(define fun-args (make-hash))
;; Defines a hashset of functions seen. Used for terminating the recursion at
;; a given depth
(define func-hash (make-hash))
(define (create/get-func func)
  (let ([val (hash-ref func-hash func (void))])
    (cond
     [(void? val) (begin (hash-set! func-hash func #t) #f)]
     [else val])))

(define (symbolic-exec-fun decl depth . args)
  (letrec ([funName (cdar decl)]
	   [funArgs (cddar decl)]
	   [funBody (cdddar decl)])
    (begin 
      (create/get-func funName)
      (hash-set! fun-bodies funName funBody)
      (hash-set! fun-args funName funArgs)
      (symbolic-exec funBody (make-hash (zip funArgs args)) depth))))

(define (symbolic-exec body expected/actual-args depth)
  (cond
   [(list? decl)
    (match (car decl)
      ['App
       (letrec ([funVisited (hash-ref func-hash funName (void))]
		[funName (cdar decl)]
		[actualArgs (cddar decl)])
	 (cond 
	  [(not (term? funName)) (apply funName actualArgs)]
	  [else (let ([(execDepth (lambda (d)
				    (symbolic-exec
				     (hash-ref fun-bodies funName #t)
				     (make-hash (zip (hash-ref fun-args funName '()) actualArgs))
				     depth)))])
		  (match funVisited 
		    [#t (cond
			 [(eq? depth 0) (void)]
			 [else (execDepth (depth - 1))])]
		    [#f (execDepth depth)]))]))]
      ['If ()])]
   ;; Is not a complex statement, it either needs to be evaluated further or substituted
   [else
    (let ([maybe-arg (hash-ref expected/actual-args decl (void))])
      (cond
       [(void? maybe-arg) decl]
       [else maybe-arg]))]))


;; Returns a list of symbolic paths and their constraints, which can be used to match to a
;; top level definition
(define (symbolic-exec decl top-level)
  (cond 
   ;; TODO: recursion
   ;; If a function decl is seen, record the name for recursion, and start symbolic execution
   [(and (list? decl) (eq? 'Fun (car decl)))
    (begin (create/get-func (list-ref decl 1))
	   (symbolic-exec (list-ref decl 3) top-level))]
   ;; If an app is seen, check if recursion is about to occur
   [(and (list? decl) (eq? 'App (car decl)))
    (let ([fun (list-ref decl 1)])
      (cond 
       ;; Todo: handle case where fun is non-symbolic, just apply
       [(symbolic? fun) fun]
       ;; Todo: handle case where fun is symbolic, try recursion
       [else
	(let
	    ([seenFBefore (create/get-func (list-ref decl 1))])
	  (cond
	   [seenFBefore (#t)]
	   ;; TODO: recurse properly by passing in the symbolic vars to be manipulated to the
	   ;; recursion
	   [else (symbolic-exec (list-ref decl 3) top-level)]))]))]
   
   ;; If an if is seen, get the path constraints and require the paths to match them
   [(and (list? decl) (eq? 'If (car decl)))
    (letrec
	;; TODO: handle constraints with recursion
	([trueConstaint (symbolic-exec (list-ref decl 1) (void))]
	 [falseConstaint (not trueConstaint)])
      (list
       (and trueConstaint (symbolic-exec (list-ref decl 2) top-level))
       (and falseConstaint (symbolic-exec (list-ref decl 2) top-level))))]
   ;; If this is a possible tree end of the expression evaluation of the function,
   ;; then check equality with the top-level symbolic var, else, simply leave it
   [else (cond
	  [(void? top-level) decl]
	  [else (apply = (list top-level decl))])]))
