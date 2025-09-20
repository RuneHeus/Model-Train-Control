#lang racket

(require racket/date)
(require racket/tcp)
(require racket/gui)

(require "Procedures.rkt")
(require "NMBS/NMBS.rkt")
(require "Infrabel/Infrabel.rkt")

;Importing connections
(require "NMBS/Connection.rkt")
(require "Infrabel/Connection.rkt")

(define file-checked? #f)

(define infrabel (make-infrabel))

(define connection-to-infrabel (make-infrabel-connection infrabel))

(define nmbs (make-nmbs))

(define connection-to-nmbs (make-nmbs-connection nmbs))

(define timer
  (new timer%
       [notify-callback
        (lambda ()
          ((connection-to-infrabel 'listen-for-clients))
          (when (not (nmbs 'started?))
            ((nmbs 'start)))
          (when (and (connection-to-infrabel 'nmbs-connection-in) (not (infrabel 'started?)))
            ((infrabel 'start) connection-to-infrabel))
          ((connection-to-infrabel 'update-infrabel))
          ((connection-to-nmbs 'update-nmbs))
          (when (and (= (nmbs 'setup-count) 2) (not file-checked?))
            ;Check if there is anything inside the save_scenario.txt file (Extra voor fase 3)
            (when (file-exists? "saved_scenario.txt")
              (define file (open-input-file "saved_scenario.txt"))

              (when (not (eof-object? (read-byte file)))
                (let* ((data-list (read-data "saved_scenario.txt"))
                       (Trains (get-data-by-category 'Trains data-list))
                       (Lights (get-data-by-category 'Lights data-list))
                       (Barriers (get-data-by-category 'Barriers data-list))
                       (Switches (get-data-by-category 'Switches data-list))
                       (Blocks (get-data-by-category 'Blocks data-list))
                       (gui (nmbs 'get-gui)))
                  (for-each (lambda (train)
                              ((gui 'add-train!) (car train) (cadr train) (caddr train))
                              (let ((train-adt (search-adt (car train) (gui 'get-train-list))))
                                ((train-adt 'set-destination!) (cadddr train))))
                            Trains)
                  (for-each (lambda (light)
                              ((gui 'send-light-status) (search-adt (car light) (gui 'get-lights)) (cadr light)))
                            Lights)
                  (for-each (lambda (barrier)
                              (let ((barrier-adt (search-adt (car barrier) (first-elements (gui 'get-barriers-list)))))
                                ((barrier-adt 'set-status!) (cadr barrier))))
                            Barriers)
                  (for-each (lambda (switch)
                              (let ((switch-adt (search-adt (car switch) (gui 'get-switches)))
                                    (id (car switch))
                                    (in (cadr switch))
                                    (out (caddr switch))
                                    (not-out (cadddr switch)))
                                (when (not (eq? out (switch-adt 'out1)))
                                  ((switch-adt 'change-status!) #t)
                                  ((gui 'send-switch-value) switch-adt #t))))
                            Switches)
                  (for-each (lambda (block)
                              (let ((block-adt (search-adt (car block) (gui 'get-detectionblocks)))
                                    (train (cadr block))
                                    (next-trains (return-train-adt-list (caddr block) (gui 'get-train-list)))
                                    (occupied? (cadddr block)))
                                ((block-adt 'add-train!) train)
                                ((block-adt 'set-next-trains-list) next-trains)
                                ((block-adt 'set-occupied!) occupied?)))
                            Blocks)))
              (set! file-checked? #t))))]
       [interval 10]))

;(tcp-close listener)
;(close-input-port in)
;(close-output-port out)

(send timer start 10)