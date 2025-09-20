#lang racket

;Connection = FIXED

(provide make-nmbs-light)

(define (make-nmbs-light id code connection)

  ((connection 'send-message) (list 'Infrabel 'add-light! id))
  ((connection 'send-message) (list 'Interface 'set-sign-code (list id code)))
  
  (define (set-code! val connection)
    ((connection 'send-message) (list 'Light 'set-code! (list id val)))
    (set! code val))

  (define (dispatch msg)
    (cond ((eq? msg 'get-id) id)
          ((eq? msg 'set-code!) set-code!)
          ((eq? msg 'get-code) code)
          (else (error "No message found for NMBS/Light.rkt"))))
  
  dispatch)