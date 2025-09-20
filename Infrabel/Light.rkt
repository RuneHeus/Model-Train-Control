#lang racket

(require "../simulator/simulator/interface.rkt")
;(require "../hardware-library/interface.rkt")

(provide make-infrabel-light)

(define (make-infrabel-light id)

  (define (set-code! val)
    (set-sign-code! id (cadr val)))

  (define (dispatch msg)
    (cond ((eq? msg 'get-id) id)
          ((eq? msg 'set-code!) set-code!)
          (else (display "Error: Infrabel/Light.rkt => ") (display msg))))
  dispatch)