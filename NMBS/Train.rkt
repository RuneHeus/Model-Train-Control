#lang racket

;Connection = FIXED

(provide make-nmbs-train)

(define (make-nmbs-train id pos speed connection) ;Dit is de nmbs representatie van de treinen
  (let ((prev-speed speed)
        (destination #f))

    ;Message to infrabel
    ((connection 'send-message) (list 'Infrabel 'add-train! (list id pos speed)))
 
    (define (change-speed! spd)
      ((connection 'send-message) (list 'Train 'set-speed! (list id spd)))
      (set! speed spd))

    (define (set-prev-speed! spd)
      (set! prev-speed spd))

    (define (set-destination! dest)
      (set! destination dest))

    (define (to-string)
      (display "Train: ")
      (display id)
      (display " "))

    (define (set-pos! position)
      (set! pos position))
  
    (define (dispatch msg)
      (cond ((eq? msg 'change-speed!) change-speed!)
            ((eq? msg 'get-speed) speed)
            ((eq? msg 'set-pos!) set-pos!)
            ((eq? msg 'get-pos) pos)
            ((eq? msg 'get-id) id)
            ((eq? msg 'to-string) to-string)
            ((eq? msg 'get-prev-speed) prev-speed)
            ((eq? msg 'set-prev-speed!) set-prev-speed!)
            ((eq? msg 'get-destination) destination)
            ((eq? msg 'set-destination!) set-destination!)
            (else (display "Error: NMBS/Train.rkt => ") (display msg))))
    dispatch))