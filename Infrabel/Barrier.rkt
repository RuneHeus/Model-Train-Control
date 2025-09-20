#lang racket

(require "../simulator/simulator/interface.rkt")
;(require "../hardware-library/interface.rkt")

(provide make-infrabel-barrier)

(define (make-infrabel-barrier id)

  (define (set-status! bool)
    (if (cadr bool) ;Closed?
        (close-crossing! id)
        (open-crossing! id)))
  
  (define (dispatch msg)
    (cond ((eq? msg 'set-status!) set-status!)
          ((eq? msg 'get-id) id)
          (else (display "Error: Infrabel/Barrier.rkt => ") (display msg))))
  dispatch)