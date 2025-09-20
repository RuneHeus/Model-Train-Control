#lang racket

(require "../Procedures.rkt")

(provide make-nmbs-connection)

(define (make-nmbs-connection nmbs)
  (let ((adt car)
        (procedure cadr)
        (values caddr))

    (define-values (in out) (tcp-connect "localhost" 12345))

    (define (send-message msg)
      (write msg out)
      (flush-output out))

    (define (update-nmbs)
      (when (byte-ready? in)
        (let ((message (read in)))
          (cond ((eq? (adt message) 'NMBS) ((nmbs (procedure message)) (values message)))
                ((eq? (adt message) 'Detectionblock)
                 (if (eq? (car (values message)) 'add-train!)
                     (((search-adt (car (values message)) ((nmbs 'get-gui) 'get-detectionblocks)) (procedure message)) (search-adt (car (cdr (values message))) ((nmbs 'get-gui) 'get-train-list)))
                     (((search-adt (car (values message)) ((nmbs 'get-gui) 'get-detectionblocks)) (procedure message)) (car (cdr (values message))))))
                (else (display "Error: NMBS/Connection.rkt => ") (display message))))))

    (define (set-train-speed! id val)
      (write val out)
      (flush-output out))
    
    (define (dispatch msg)
      (cond ((eq? msg 'update-nmbs) update-nmbs)
            ((eq? msg 'in) in)
            ((eq? msg 'out) out)
            ((eq? msg 'send-message) send-message)
            ((eq? msg 'set-train-speed!) set-train-speed!)
            (else (error "No message found for NMBS/Connection.rkt"))))
    
    dispatch))