#lang racket

(provide open-input-list)

;; Maps a symbolic list to an input port
(define (open-input-list lst)
  (open-input-string (string-join (map ~a lst) " ")))
