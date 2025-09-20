#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;               - Youri Coppens: AI Lab: VUB - 2023
;
;       simulator.rkt
;
;       Main simulator loop. Runs in a separate thread.
;

(require racket/gui/base racket/class)

(require  "graphics.rkt"
          (only-in "railway.rkt" the-railway [clear-setup rw:clear-setup])
          (only-in "trains.rkt" TRAINS [remove-all-trains tr:remove-all-trains]))

(provide initialize-simulator launch-simulator-loop stop-simulator-loop request-redraw-switch)

(define WINDOW #f)
(define REDRAW-SWITCHES '())

(define CONTINUE? #t)

(define WINDOW-TITLE "GUI Simulator")
(define SCREEN-WIDTH 3000) 
(define SCREEN-HEIGHT 1800)

(define DESIRED-MAX-FPS 25) ;; Lower this value to get less CPU usage

(define (draw-all-segments)
  (send the-railway draw-all-segments WINDOW 'segments))

(define (draw-all-trains)
  (for-each (lambda (train)
              (send train draw WINDOW 'trains))
            TRAINS))

(define (draw-all-crossings)
  (send the-railway draw-all-crossings WINDOW 'crossings))

(define (draw-all-signs)
  (send the-railway draw-all-signs WINDOW 'signs))

(define (draw-switch switch-id)
  (send the-railway draw-switch switch-id WINDOW 'segments))

(define (request-redraw-switch switch-id)
  (set! REDRAW-SWITCHES (cons switch-id REDRAW-SWITCHES)))

(define (initialize-simulator)
  (unless WINDOW
    (set! WINDOW (make-object window% WINDOW-TITLE SCREEN-WIDTH SCREEN-HEIGHT on-close-callback))
    (sleep/yield 1)
    (send WINDOW create-layer 'segments)
    (send WINDOW create-layer 'crossings)
    (send WINDOW create-layer 'signs)
    (send WINDOW create-layer 'trains)))

(define (clear-internals)
  (tr:remove-all-trains)
  (rw:clear-setup))

(define (clear-gui)
  (send WINDOW clear-all-layers)
  (send WINDOW refresh))

(define (end-simulator)
  (clear-internals)
  (clear-gui))

(define (launch-simulator-loop)
  (let ((wait-per-frame (exact-round (/ 1000 DESIRED-MAX-FPS)))
        (previous-time (current-inexact-milliseconds))
        (delta-time 0)
        (game-loop-timer '()))

    (define (simulator-loop)
      (define curr-time (current-inexact-milliseconds))
      (set! delta-time (- curr-time previous-time))
      (set! previous-time curr-time)
      (send WINDOW set-frame-title (format "~a - FPS: ~a"  WINDOW-TITLE  (exact-round (/ 1000 delta-time))))
      (send WINDOW suspend-flush)
      (send WINDOW clear-layer 'trains)
      (send WINDOW clear-layer 'crossings)
      (send WINDOW clear-layer 'signs)
      (unless (null? REDRAW-SWITCHES)
        (for-each draw-switch REDRAW-SWITCHES)
        (set! REDRAW-SWITCHES '()))
      (with-handlers ([exn:fail? (lambda (exn)  ; capture crashing trains so that main event doesn't die
                                   ((error-display-handler) (exn-message exn) exn) ; still print the error
                                   (send WINDOW refresh)
                                   (send WINDOW resume-flush)
                                   (clear-internals))])   
        (for-each (lambda (train)
                    (send train move (/ delta-time 1000))) ; trains might crash!
                  TRAINS))
      (draw-all-trains)
      (draw-all-crossings)
      (draw-all-signs)
      (send WINDOW refresh)
      (send WINDOW resume-flush)
      (if CONTINUE?
          (send game-loop-timer start (max (exact-round (- wait-per-frame (- (current-inexact-milliseconds) curr-time))) 0) #t)
          (end-simulator)))

    (unless the-railway
      (error "Please set up a railway before launching the simulator"))
    (send the-railway build SCREEN-WIDTH SCREEN-HEIGHT)
    (clear-gui) ; in case we recover from train crash
    (draw-all-segments)
    (set! CONTINUE? #t)
    (set! game-loop-timer
          (new timer% [notify-callback simulator-loop]))
    (send game-loop-timer start wait-per-frame #t)))

(define (stop-simulator-loop)
  (set! CONTINUE? #f))

(define (on-close-callback)
  (stop-simulator-loop)
  (sleep/yield 1) ;let the game-loop-timer finish
  (set! WINDOW #f))
