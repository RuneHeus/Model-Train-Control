#lang racket

;Connection = FIXED

(provide make-nmbs-barrier)

;#t = Closed
;#f = Open

(define (make-nmbs-barrier id closed? connection)

  ((connection 'send-message) (list 'Infrabel 'add-barrier! id))

  (define (set-status! bool)
    (set! closed? bool)
    ((connection 'send-message) (list 'Barrier 'set-status! (list id bool))))

  (define (dispatch msg)
    (cond ((eq? msg 'set-status!) set-status!)
          ((eq? msg 'get-id) id)
          ((eq? msg 'closed?) closed?)
          (else (display "Error: NMBS/Barrier.rkt => ") (display msg))))
  dispatch)