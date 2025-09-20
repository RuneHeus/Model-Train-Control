#lang racket

(require "../Procedures.rkt")
(require "../simulator/simulator/interface.rkt")
;(require "../hardware-library/interface.rkt")

(provide make-infrabel-connection)

(define (make-infrabel-connection infrabel)
  (let ((adt car)
        (procedure cadr)
        (values caddr)
        (listener (tcp-listen 12345)) ;Server
        (nmbs-in #f)
        (nmbs-out #f)
        (nmbs-connection-in #f)
        (nmbs-connection-out #f)
        )

    (define (send-message msg)
      (write msg nmbs-connection-out) ;Writing to NMBS connection
      (flush-output nmbs-connection-out))

    (define (listen-for-clients)
      (when (tcp-accept-ready? listener)
        (define-values (in out) (tcp-accept listener))
        (if (not nmbs-in)
            (begin (set! nmbs-in in) (set! nmbs-out out))
            (begin (set! nmbs-connection-in in) (set! nmbs-connection-out out)))))
    
    (define (update-infrabel) ;Loop through messages for infrabel
      (when nmbs-in
        (when (byte-ready? nmbs-in)
          (let ((message (read nmbs-in)))
            (cond ((eq? (adt message) 'Infrabel)          ((infrabel (procedure message)) (values message)))
                  ((eq? (adt message) 'Train)             (((search-adt (car (values message)) (infrabel 'get-trains))            (procedure message)) (values message)))
                  ((eq? (adt message) 'Switch)            (((search-adt (car (values message)) (infrabel 'get-switches))          (procedure message)) (values message)))
                  ((eq? (adt message) 'Barrier)           (((search-adt (car (values message)) (infrabel 'get-barriers))          (procedure message)) (values message)))
                  ((eq? (adt message) 'Light)             (((search-adt (car (values message)) (infrabel 'get-lights))            (procedure message)) (values message)))
                  ((eq? (adt message) 'Detectionblock)    (((search-adt (car (values message)) (infrabel 'get-detectionblocks))   (procedure message)) (values message)))
                  ((eq? (adt message) 'Interface)         (eval-interface-procedures (car (cdr message)) (car (cdr (cdr message)))))
                  ((eq? (adt message) 'NMBS)              (if (eq? (procedure message) 'detectionblock-loop)
                                                              (let* ((occupied (get-occupied-detection-blocks))
                                                                     (msg (list 'NMBS 'detectionblock-loop occupied)))
                                                                (send-message msg))
                                                              (send-message message)))
                  (else (display "Error: Infrabel/Connection.rkt => ") (display message)))))))

    (define (eval-interface-procedures procedure args)
      (cond ((eq? procedure 'set-sign-code!) (set-sign-code! (car args) (car (cdr args))))))

    (define (dispatch msg)
      (cond ((eq? msg 'update-infrabel) update-infrabel)
            ((eq? msg 'listen-for-clients) listen-for-clients)
            ((eq? msg 'send-message) send-message)
            ((eq? msg 'nmbs-connection-in) nmbs-connection-in)
            (else (error "No message found for Infrabel/Connection.rkt"))))
    dispatch))