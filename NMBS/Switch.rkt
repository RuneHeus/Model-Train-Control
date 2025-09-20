#lang racket

;Connection = FIXED

(provide make-nmbs-switch)

(define (make-nmbs-switch id in out1 out2 connection)
  (let ((connection-in in)
        (connected-out out1)
        (not-connected-out out2)
        (value #f))

    ((connection 'send-message) (list 'Infrabel 'add-switch! id))

    (define (to-string)
      (display "Switch: ")
      (display id)
      (display ", in: ")
      (display in)
      (display ", out1: ")
      (display out1)
      (display ", out2: ")
      (display out2)
      (newline))
    
    (define (change-status! val)
      ((connection 'send-message) (list 'Switch 'change-status! (list id val)))
      (set! value val)
      (let ((temp-out1 out1))
        (set! out1 out2)
        (set! out2 temp-out1)))
  
    (define (dispatch msg)
      (cond ((eq? msg 'get-id) id)
            ((eq? msg 'change-status!) change-status!)
            ((eq? msg 'connection-in) connection-in)
            ((eq? msg 'connected-out) connected-out)
            ((eq? msg 'in) in)
            ((eq? msg 'out1) out1)
            ((eq? msg 'out2) out2)
            ((eq? msg 'not-connected-out) not-connected-out)
            ((eq? msg 'to-string) to-string)
            ((eq? msg 'get-value) value)
            (else (error (~a msg " is not an argument")))))
    dispatch))