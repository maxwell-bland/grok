#lang racket

(require racket/file)
(require "./interpreter.rkt")

(provide main)

(define (main . args)
  (cond
   ;; Check number of arguments, if a single file, read & interpret s-expression from file
   [(not (eq? (length args) 2)) error "Incorrect number of arguments supplied."]
   [else (begin
	   (define in (open-input-file (list-ref args 0) #:mode 'text))
	   (interpret in (list-ref args 1)))]))


