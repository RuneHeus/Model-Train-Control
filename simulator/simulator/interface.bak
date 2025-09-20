#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;               - Youri Coppens: AI LAB: VUB - 2023
;
;       interface.rkt
;
;       main interface to the simulator, import this file
;

(require "trains.rkt"
         "railway.rkt"
         "simulator.rkt")

(provide setup-hardware
         setup-straight
         setup-straight-with-switch
         setup-loop
         setup-loop-and-switches

         start
         stop

         get-detection-block-ids
         get-switch-ids
         
         add-loco
         remove-loco
         
         get-loco-speed
         set-loco-speed!

         get-occupied-detection-blocks
         
         get-switch-position
         set-switch-position!

         close-crossing!
         open-crossing!

         set-sign-code!)

;;; INTERFACE ;;;


;; ========================================================================== ;;
;;                                                                            ;;
;; (setup-hardware)                                                           ;;
;; (setup-straight)                                                           ;;
;; (setup-straight-with-switch)                                               ;;
;; (setup-loop)                                                               ;;
;; (setup-loop-and-switches)                                                  ;;
;;                                                                            ;;
;; initializes the simulator with a particular setup; check the pdf files     ;;
;; for an overview of all block and switch ids.                               ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (setup-hardware)                                                           ;;
;;                                                                            ;;
;; ========================================================================== ;;

;; ========================================================================== ;;
;;                                                                            ;;
;; (start)                                                                    ;;
;;                                                                            ;;
;; Starts the simulator. The framerate can be adjusted by altering            ;;
;; DESIRED-MAX-FPS in simulator.rkt.                                          ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (start)
  (initialize-simulator)
  (launch-simulator-loop))

;; ========================================================================== ;;
;;                                                                            ;;
;; (stop)                                                                     ;;
;;                                                                            ;;
;; Stops the simulator.                                                       ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define stop stop-simulator-loop)

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-detection-block-ids)                                                  ;;
;;                                                                            ;;
;; Returns a list of all symbols of detection block ids, unless no setup      ;;
;; has been initialized.                                                      ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-detection-block-ids)                                                  ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-detection-block-ids)
  (if the-railway
      (send the-railway get-detection-block-ids)
      (error "Please set up a railway first!")))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-switch-ids)                                                           ;;
;;                                                                            ;;
;; Returns a list of all symbols of switch ids, unless no setup has been      ;;
;; initialized.                                                               ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-switch-ids)                                                           ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-switch-ids)
  (if the-railway
      (send the-railway get-switch-ids)
      (error "Please set up a railway first!")))

;; ========================================================================== ;;
;;                                                                            ;;
;; (add-loco TRAIN-ID PREVIOUS-SEGMENT-ID CURRENT-SEGMENT-ID)                 ;;
;;                                                                            ;;
;; Adds a train to the segment with id CURRENT-SEGMENT-ID. The direction of   ;;
;; the train is determined by PREVIOUS-SEGMENT-ID.                            ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (add-loco 'T-1 'S-27 '1-3)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (add-loco train-id previous-segment-id current-segment-id)
  (if the-railway
      (add-train train-id previous-segment-id current-segment-id)
      (error "Please set up a railway first!")))

;; ========================================================================== ;;
;;                                                                            ;;
;; (remove-loco TRAIN-ID)                                                     ;;
;;                                                                            ;;
;; Removes the train from the tracks                                          ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (remove-loco 'T-1)                                                         ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define remove-loco remove-train)

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-loco-speed TRAIN-ID)                                                  ;;
;;                                                                            ;;
;; Returns the current speed of the train with id TRAIN-ID                    ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-loco-speed 'T-1)                                                      ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define get-loco-speed get-train-speed)

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-loco-speed! TRAIN-ID SPEED)                                           ;;
;;                                                                            ;;
;; Sets the speed of the train in mm/s with id TRAIN-ID to SPEED. Setting the ;;
;; speed to a negative number will invert the direction of the train.         ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (set-loco-speed! 'T-1 100)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define set-loco-speed! set-train-speed!)

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-occupied-detection-blocks)                                            ;;
;;                                                                            ;;
;; Returns the id's of detection blocks on which a train is on.               ;;
;; If no trains are currently detected, an empty list will be returned.       ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-occupied-detection-blocks)                                            ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define get-occupied-detection-blocks get-train-detection-blocks)

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-switch-position! SWITCH-ID POSITION)                                  ;;
;;                                                                            ;;
;; Sets the switch with id SWITCH-ID to POSITION. Only 1 and 2 are valid      ;;
;; position numbers. Consult opstelling_schema.pdf to get an overview of all  ;;
;; switch positions. Consult railway.rkt for an overview of all switch ids.   ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (set-switch-position! 'S-9 2)                                              ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (set-switch-position! id position)
  (if the-railway
      (begin (request-redraw-switch id)
             (send the-railway set-switch-position! id position))
      (error "Please set up a railway first!")))
      


;; ========================================================================== ;;
;;                                                                            ;;
;; (get-switch-position SWITCH-ID)                                            ;;
;;                                                                            ;;
;; Returns the position of the switch with id SWITCH-ID. Consult railway.rkt  ;;
;; for an overview of all switch ids.                                         ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-switch-position 'S-9)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-switch-position id)
  (if the-railway
      (send the-railway get-switch-position id)
      (error "Please set up a railway first!")))



;; ========================================================================== ;;
;;                                                                            ;;
;; (close-crossing CROSSING-ID)                                               ;;
;;                                                                            ;;
;; Closes the crossing with id CROSSING-ID                                    ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (close-crossing 'C-1)                                                      ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (close-crossing! id)
  (if the-railway
      (send the-railway close-crossing id)
      (error "Please set up a railway first!")))


;; ========================================================================== ;;
;;                                                                            ;;
;; (open-crossing CROSSING-ID)                                                ;;
;;                                                                            ;;
;; Opens the crossing with id CROSSING-ID                                     ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (open-crossing 'C-1)                                                       ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (open-crossing! id)
  (if the-railway
      (send the-railway open-crossing id)
      (error "Please set up a railway first!")))

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-sign-code SIGN-ID CODE)                                               ;;
;;                                                                            ;;
;; Sets the sign with id SIGN-ID to CODE. Only certain code signals are       ;;
;; valid, namely:                                                             ;;
;;     1. Hp0 (red)                                                           ;;
;;     2. Hp1 (green)                                                         ;;
;;     3. Hp0+Sh0 (white - green)                                             ;;
;;     4. Ks1+Zs3 (green - eight)                                             ;;
;;     5. Ks2 (orange - white)                                                ;;
;;     6. Ks2+Zs3 (orange - white - eight)                                    ;;
;;     7. Sh1 (white)                                                         ;;
;;     8. Ks1+Zs3+Zs3v (green - white - eight -six)                           ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (set-sign-code 'L-1 'Hp0+Sh0)                                              ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (set-sign-code! id code)
  (if the-railway
      (send the-railway set-sign-code id code)
      (error "Please set up a railway first!")))