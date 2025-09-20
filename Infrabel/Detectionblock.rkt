#lang racket

(require "../simulator/simulator/interface.rkt")
;(require "../hardware-library/interface.rkt")

(provide make-infrabel-detectionblock)

(define (make-infrabel-detectionblock id)
  
  (define (dispatch msg)
    (cond ((eq? msg 'get-id) id)
          (else (display "Error: Infrabel/Detectionblock.rkt => ") (display msg))))
  dispatch)