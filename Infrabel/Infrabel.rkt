#lang racket

;Connection = FIXED

(require "../simulator/simulator/interface.rkt")
;(require "../hardware-library/interface.rkt")

(require "Train.rkt")
(require "Switch.rkt")
(require "Barrier.rkt")
(require "Light.rkt")
(require "Detectionblock.rkt")

(provide make-infrabel)

(define (make-infrabel)
  (let ((started? #f)
        (trains '())
        (switches '())
        (barriers '())
        (lights '())
        (detectionblocks '()))

    (define (add-train! id-pos)
      (set! trains (append trains (list (make-infrabel-train (car id-pos) (cadr id-pos) (caddr id-pos))))))

    (define (add-switch! id)
      (set! switches (append switches (list (make-infrabel-switch id)))))

    (define (add-barrier! id)
      (set! barriers (append barriers (list (make-infrabel-barrier id)))))

    (define (add-light! id)
      (set! lights (append lights (list (make-infrabel-light id)))))

    (define (add-detectionblock! id)
      (set! detectionblocks (append detectionblocks (list (make-infrabel-detectionblock id)))))

    (define (remove-train! id)
      (remove-loco id))

    (define (start-infrabel connection)
      (setup-hardware)
      (start) ;Starting of the interface
      ((connection 'send-message)  (list 'NMBS 'setup-switches (get-switch-ids)))
      ((connection 'send-message)  (list 'NMBS 'setup-detectionblocks (get-detection-block-ids)))
      (set! started? #t))
    
    (define (dispatch msg)
      (cond ((eq? msg 'add-train!) add-train!)
            ((eq? msg 'add-switch!) add-switch!)
            ((eq? msg 'add-barrier!) add-barrier!)
            ((eq? msg 'add-light!) add-light!)
            ((eq? msg 'add-detectionblock!) add-detectionblock!)
            ((eq? msg 'remove-train!) remove-train!)
            ((eq? msg 'get-trains) trains)
            ((eq? msg 'get-switches) switches)
            ((eq? msg 'get-barriers) barriers)
            ((eq? msg 'get-lights) lights)
            ((eq? msg 'get-detectionblocks) detectionblocks)
            ((eq? msg 'start) start-infrabel)
            ((eq? msg 'started?) started?)
            (else (display "Error: Infrabel.rkt => ") (display msg))))
   
    dispatch))