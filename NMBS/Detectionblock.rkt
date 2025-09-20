#lang racket

;Connection = FIXED

(provide make-nmbs-detectionblock)

(define (make-nmbs-detectionblock id train next-trains occupied? connection)

  ((connection 'send-message) (list 'Infrabel 'add-detectionblock! id))

  (define (add-train! trn)
    (set! train trn))

  (define (remove-train!)
    (set! train #f))

  (define (set-next-trains-list lst)
    (set! next-trains lst))

  (define (add-next! next)
    (when (not (member next next-trains)) ; Check if the train already exists in the list
        (set! next-trains (append next-trains (list next)))))

  (define (remove-first-next!)
    (set! next-trains (cdr next-trains)))

  (define (remove-next-train! train)
    (define (iter lijst)
      (cond ((null? lijst) '())
            ((eq? ((car lijst) 'get-id) (train 'get-id)) (iter (cdr lijst)))
            (else (cons (car lijst) (iter (cdr lijst))))))
    (set! next-trains (iter next-trains)))

  (define (set-occupied! val)
    (set! occupied? val))

  (define (next-trains-to-string)
    (display "(")
    (for-each (lambda (train)
                ((train 'to-string)))
              next-trains)
    (display ")"))

  (define (to-string)
    (display "ID: ")
    (display id)
    
    (display ", Current Train: ")
    (if train
        (display train)
        (display #f))
    
    (display ", Next Train: ")
    (next-trains-to-string)
    
    (display ", Occupied?: ")
    (display occupied?)
    (newline)
    )
  
  (define (dispatch msg)
    (cond ((eq? msg 'occupied?) occupied?)
          ((eq? msg 'set-occupied!) set-occupied!)
          ((eq? msg 'add-train!) add-train!)
          ((eq? msg 'remove-train!) remove-train!)
          ((eq? msg 'train) train)
          ((eq? msg 'next-trains) next-trains)
          ((eq? msg 'add-next!) add-next!)
          ((eq? msg 'remove-first-next!) remove-first-next!)
          ((eq? msg 'remove-next-train!) remove-next-train!)
          ((eq? msg 'get-id) id)
          ((eq? msg 'type) 'detectionblock)
          ((eq? msg 'to-string) to-string)
          ((eq? msg 'next-trains-to-string) next-trains-to-string)
          ((eq? msg 'set-next-trains-list) set-next-trains-list)
          (else (display "Error: NMBS/Detectionblock.rkt => ") (display msg))))
  dispatch)