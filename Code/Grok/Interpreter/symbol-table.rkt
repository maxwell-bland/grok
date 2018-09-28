#lang rosette

(provide create/get-ident)

;; List of already bound idents
(define ident-hash (make-hash))

;; Looks into the list of bound idents to see if a symbolic binding
;; is already defined, if not, it creates one, if so, it returns it
;; TODO: Include type identifier
(define (create/get-ident id)
  (let ([val (hash-ref ident-hash id (void))])
    (cond
     [(string=? id "True") #t]
     [(string=? id "False") #f]
     [(void? val) (begin
		    (define-symbolic* ident integer?)
		    (hash-set! ident-hash id ident)
		    ident)]
     [else val])))
