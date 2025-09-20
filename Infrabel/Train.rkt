#lang racket

(require "../simulator/simulator/interface.rkt")
;(require "../hardware-library/interface.rkt")

(provide make-infrabel-train)

(define (make-infrabel-train id position speed)
  
  (add-loco id 'S-26 position)
  (set-loco-speed! id speed)

  (define (set-speed! val)
    (set-loco-speed! id (cadr val)))
  
  (define (dispatch msg)
    (cond ((eq? msg 'speed) (get-loco-speed id))
          ((eq? msg 'set-speed!) set-speed!)
          ((eq? msg 'get-id) id)))
  dispatch)