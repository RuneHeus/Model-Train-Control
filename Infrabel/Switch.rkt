#lang racket

(require "../simulator/simulator/interface.rkt")
;(require "../hardware-library/interface.rkt")

(provide make-infrabel-switch)

(define (make-infrabel-switch id)

  (set-switch-position! id 1)

  (define (change-status! val)
    (if (cadr val)
        (set-switch-position! id 2)
        (set-switch-position! id 1)))

  (define (dispatch msg)
    (cond ((eq? msg 'change-status!) change-status!)
          ((eq? msg 'get-id) id)))
  dispatch)