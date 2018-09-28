#lang rosette

(provide symbolic-exec)

;; Returns a list of paths which the top level should be
;; equal to
(define (symbolic-exec decl top-level)
  (cond 
    ;; TODO: recursion
    [(and (list? decl) (eq? 'Fun (car decl))) (symbolic-exec (list-ref decl 3) top-level)]
    [(and (list? decl) (eq? 'If (car decl))) (map (and (symbolic-exec (list-ref 1) (void)))
	      (list
	       (symbolic-exec (list-ref decl 2) top-level)
	       (symbolic-exec (list-ref decl 3) top-level)))]
    [else (cond 
	    [(void? top-level) decl]
	    [else (= top-level decl)])]))
