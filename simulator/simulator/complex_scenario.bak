#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;
;       scenario.rkt
;
;       Example scenario.
;

(require "interface.rkt")

(define (set-speed-at train detection-block speed)
  (if (member detection-block (get-occupied-detection-blocks))
      (set-loco-speed! train speed)
      (begin (sleep 2)
             (set-speed-at train detection-block speed))))

;; 1-5 => 2-4 => 1-6 => 2-3 => 1-8 => 2-2 => 2-1 => 2-7
(define (test-route)
  (define train 'TEST-ROUTE-TRAIN)
  (add-loco train 'S-26 '1-4)
  
  ;; 1-4 => 1-5
  (set-loco-speed! train 200)
  (set-sign-code! 'L-2 'Hp0)
  (set-speed-at train '1-5 0)
  (close-crossing! 'C-2)
  (sleep 6)

  ;; 1-5 => 2-4
  (set-sign-code! 'L-2 'Hp1)
  (set-loco-speed! train 200)
  (set-switch-position! 'S-20 1)
  (set-speed-at train '2-4 (- (get-loco-speed train)))

  ;; 2-4 => 1-6
  (open-crossing! 'C-2)
  (set-switch-position! 'S-20 2)
  (set-switch-position! 'S-6 2)
  (set-switch-position! 'S-5 1)
  (set-speed-at train '1-6 (- (get-loco-speed train)))

  ;; 1-6 => 2-3
  (set-switch-position! 'S-6 1)
  (set-speed-at train '2-3 (- (get-loco-speed train)))

  ;; 2-3 => 1-8
  (set-switch-position! 'S-5 2)
  (set-switch-position! 'S-7 1)
  (set-switch-position! 'S-25 1)
  (set-speed-at train '1-8 (- (get-loco-speed train)))

  ;; 1-8 => 2-2
  (set-switch-position! 'S-25 2)
  (set-switch-position! 'S-1 2)
  (set-switch-position! 'S-2 2)
  (set-switch-position! 'S-3 1)
  (set-speed-at train '2-2 (- (get-loco-speed train)))

  ;; 2-2 => 2-1
  (set-switch-position! 'S-1 1)
  (set-speed-at train '2-1 (- (get-loco-speed train)))

  ;; 2-1 => 2-7
  (set-switch-position! 'S-3 2)
  (set-switch-position! 'S-8 2)
  (set-switch-position! 'S-4 2)
  (set-speed-at train '2-7 0))

  ;; Remove the train and stop the simulator
  ;(sleep 3)
  ;(remove-loco train)
  ;(stop))

; Load the hardware track
(setup-hardware)

; Start the simulator
(start)

(thread test-route)

(get-occupied-detection-blocks)
  
