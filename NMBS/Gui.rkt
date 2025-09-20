#lang racket

(require "../Procedures.rkt")
(require "Switch.rkt")
(require "Train.rkt")
(require "Light.rkt")
(require "Detectionblock.rkt")
(require "Barrier.rkt")
(require racket/gui)
(require racket/date)

(provide make-gui)

(define (make-gui railway connection)
  (let* ((screen-width 600)
         (screen-height 500)
         (train-list '())
         (train-adt-list '())
         (slider-list '())
         (start-stop-buttons '())
         (log-list '())
         (switch-list '())
         (lights-list '())
         (barriers-list '())
         (detectionblock-list '())
         (tab-panels #f) ;This is the tab panel (parent of all panels)
         (train-panel #f)
         (speed-panel #f)
         (switches-panel #f)
         (barriers-panel #f)
         (detectionblock-panel #f)
         (lights-panel #f)
         (log-panel #f)
         (next-id 1) ;(needs to change to hardware id) variable to change train id
         (slider caddr)
         (slider-panel cadr)
         (message cadr)
         (remove-button caddr)
         (main-frame (new (class frame% (super-new)
                            (define/augment (on-close)
                              (let ((trains train-adt-list)
                                    (lights (first-elements lights-list))
                                    (barriers (first-elements barriers-list))
                                    (switches (first-elements switch-list))
                                    (blocks (first-elements detectionblock-list)))
                                (save-current-scenario trains lights barriers switches blocks "saved_scenario.txt")
                                (exit))))
                          [label "Trains speed"]
                          [width screen-width]
                          [height screen-height])))

    (define (initialize-panels-content)
      ;INITALIZE TAB PARENT
      (set! tab-panels (new tab-panel%
                            [parent main-frame]
                            [choices (list "Trains" "Speed" "Switches" "Barriers" "Detectionblocks" "Lights" "Log")]
                            [callback change-tab]))

      ;INITIALIZE TRAIN-PANEL
      (set! train-panel (new vertical-panel% [parent tab-panels] [style '(vscroll)]))

      ;ADD TRAIN BUTTON
      (new button% [label "Add Train"]
           [parent train-panel]
           [callback (lambda (button event)
                       (add-train!)
                       (set! next-id (+ next-id 1)))])

      ;INITIALIZE SPEED-PANEL
      (set! speed-panel (new vertical-panel% [parent tab-panels] [style '(vscroll)]))

      ;INITIALIZE SWITCHES-PANEL
      (set! switches-panel (new vertical-panel% [parent tab-panels] [style '(vscroll)])) 

      ;INITIALIZE BARRIERS-PANEL
      (set! barriers-panel (new vertical-panel% [parent tab-panels] [style '(vscroll)]))

      ;INITIALIZE DETECTIONBLOCK-PANEL
      (set! detectionblock-panel (new vertical-panel% [parent tab-panels] [style '(vscroll)]))

      ;INITIALIZE LIGHTS-PANEL
      (set! lights-panel (new vertical-panel% [parent tab-panels] [style '(vscroll)]))
      
      ;INITIALIZE LOG-PANEL
      (set! log-panel (new vertical-panel% [parent tab-panels] [style '(vscroll)])))
      

    ;; Function to handle tab changes
    (define (change-tab tp event)
      (when (eq? (send event get-event-type) 'tab-panel)
        (fill-tab tp (send tp get-selection))))

    ;; Function to fill the selected tab
    (define (fill-tab tp x)
      (cond
        ((eq? x 0) ; Train tab
         (send tp change-children (lambda (children)
                                    (list train-panel))))
        ((eq? x 1) ; Speed tab
         (for-each show-all-children (map slider-panel slider-list))
         (send tp change-children (lambda (children)
                                    (list speed-panel))))
        ((eq? x 2) ;Switch tab
         (show-all-children switches-panel)
         (send tp change-children (lambda (children)
                                    (list switches-panel))))
        ((eq? x 3) ;Barriers tab
         (show-all-children barriers-panel)
         (send tp change-children (lambda (children)
                                    (list barriers-panel))))
        ((eq? x 4) ;Detectionblocks tab
         (show-all-children detectionblock-panel)
         (send tp change-children (lambda (children)
                                    (list detectionblock-panel))))
        
        ((eq? x 5) ;Lights tab
         (show-all-children lights-panel)
         (send tp change-children (lambda (children)
                                    (list lights-panel))))
        ((eq? x 6) ;Log tab
         (send tp change-children (lambda (children)
                                    (list log-panel)))
         (show-all-children log-panel))))

    (define (get-user-input prompt)
      (define result #f) ; Variable to store the user's input
      (define input-dialog (new dialog% [label "Input Dialog"] [parent #f]))
      (define input-panel (new horizontal-panel% [parent input-dialog]))
      (define input-field (new text-field% [parent input-panel] [label prompt]))
      (define button-panel (new horizontal-panel% [parent input-dialog]))

      (define (ok-callback button event)
        (set! result (send input-field get-value))
        (send input-dialog show #f))

      (new button% [parent button-panel] [label "OK"] [callback ok-callback])

      (send input-dialog show #t)
      result)

    ;; Function to add a train to the list
    (define (add-train! . args)
      (let ((slider-panel (new horizontal-panel%
                               [parent speed-panel]))
            (id #f)
            (pos #f)
            (speed 0))

        (if (not (null? args))
            (begin
              (set! id (car args))
              (set! pos (cadr args))
              (set! speed (caddr args)))
            (begin
              (set! id (string->symbol (get-user-input "Give train id:")))
              (set! pos (string->symbol (get-user-input "Give train position:")))))

        (let* ((train (make-nmbs-train id pos speed connection))
               (remove-train-button (new button%
                                         [label (~a "Remove Train: " id)]
                                         [callback (lambda (button event)
                                                     (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Train " id " removed!") #f)
                                                     (remove-train! id))]
                                         [parent train-panel]))
               
               (message (new message%
                             [label (~a "Train " id ", Position: " pos)]
                             [parent train-panel]))
               (slider (new slider%
                            [min-value -200]
                            [max-value 200]
                            [label (~a "Speed of Train " id)]
                            [parent slider-panel]
                            [callback (lambda (button event)
                                        (set-train-speed! id))]))
               (start-button (new button%
                                  [label "Start"]
                                  [parent slider-panel]
                                  [callback (lambda (button event)
                                              (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Train " id " started") #t)
                                              (set-slider-value! id 200)
                                              (set-train-speed! id))]))
               (stop-button (new button%
                                 [label "Stop"]
                                 [parent slider-panel]
                                 [callback (lambda (button event)
                                             (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Train " id " stopped") #t)
                                             (set-slider-value! id 0)
                                             (set-train-speed! id))]))
               (switch-button (new button%
                                   [label "Switch"]
                                   [parent slider-panel]
                                   [callback (lambda (button event)
                                               (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Train " id " switched directions") #t)
                                               (set-slider-value! id oppesite-value)
                                               (set-train-speed! id))]))
               (location-input (new text-field%
                                    [label "Location"]
                                    [parent slider-panel]))
               
               (location-button (new button%
                                     [label "Move"]
                                     [parent slider-panel]
                                     [callback (lambda (button event)
                                                 (let* ((location (send location-input get-value))
                                                        (elements (first-elements switch-list))
                                                        (route (list ((railway 'calculate-path) (train 'get-pos) (string->symbol location) (first-elements switch-list)) id)))
                                                   (when (not (eq? location ""))
                                                     (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Train " id "'s location is being calculated") #t)
                                                     ((connection 'send-message) (list 'NMBS 'start-route route))
                                                     )))])))

          ;Add train to detectionblock 1-4
          (((search-adt pos (first-elements detectionblock-list)) 'add-train!) id)
          (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Train " id " added!") #f)
          (send log-panel show #f)
          (send slider set-value 0)
          (send slider show #f)
          (send start-button show #f)
          (send stop-button show #f)
          (send switch-button show #f)
          (send location-input show #f)
          (send location-button show #f)
          (set! train-list (append train-list (list (list id message remove-train-button))))
          (set! train-adt-list (append train-adt-list (list train)))
          (set! slider-list (append slider-list (list (list id slider-panel slider))))
          (set! start-stop-buttons (append start-stop-buttons (list (list id start-button stop-button)))))))

    (define (find-train-message id)
      (define (iter lst)
        (cond ((null? lst) #f)
              ((eq? (car (car lst)) id) (car lst))
              (else (iter (cdr lst)))))
      (iter train-list))

    (define (change-train-position! id)
      (let* ((pair (find-train-message id))
             (train (search-adt (car pair) train-adt-list))
             (message (car (cdr pair))))
        (send message set-label (~a "Train " id ", Position: " (train 'get-pos)))))

    (define (search-train-adt id)
      (define (iter lst)
        (cond ((null? lst) '())
              ((eq? ((car lst) 'get-id) id) (car lst))
              (else (iter (cdr lst)))))
      (iter train-adt-list))

    (define (inform-detectionblocks train but-det) ;This procedure informs all the detectionblocks of the same next-train id to delete the train
      (for-each (lambda (block)
                  (when (not (eq? but-det (block 'get-id)))
                    (when (and (not (null? (block 'next-trains))) (eq? (car (block 'next-trains)) train))
                      ((block 'remove-next-train!) train))))
                (first-elements detectionblock-list)))
    
    (define (set-train-speed! id)
      (let* ((sldr (slider (assq id slider-list)))
             (slider-value (send sldr get-value))
             (train (search-train-adt id)))
        (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Slider of train: " id " set to " slider-value) #f) ;Log the change
        ((train 'change-speed!) slider-value)
        ((train 'set-prev-speed!) slider-value)
        (inform-detectionblocks id '())))

    (define (add-switch! location in out1 out2) ;Adds a switch to to the panel
      (let* ((switch (make-nmbs-switch location in out1 out2 connection))
             (check-box (new check-box%
                             [label (~a "Switch " location)] 
                             [parent switches-panel]
                             [callback (lambda (button event)
                                         (send-switch-status switch))])))
        (set! switch-list (append switch-list (list (list switch check-box))))
        (send check-box show #f)))

    (define (send-switch-status switch) ;This is the function to send the value from the just clicked switch over to infrabel
      ((switch 'change-status!) (get-switch-value switch))
      (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Switch " (switch 'get-id) " changed!") #t))

    (define (send-switch-value switch val)
      (let ((pair (assq switch switch-list)))
        (send (car (cdr pair)) set-value val)))
    
    (define (get-switch-value switch)
      (let ((pair (assq switch switch-list)))
        (send (car (cdr pair)) get-value)))

    (define (add-light! id code)
      (let* ((light (make-nmbs-light id code connection))
             (list-box (new list-box%
                            [label (~a "Light " id)]
                            [parent lights-panel]
                            [choices (list "Green" "Red")]
                            [callback (lambda (button event)
                                        (send-light-status light))])))
        (set! lights-list (append lights-list (list (list light list-box))))
        (set-light-value! light 'Hp0)
        (send list-box show #f)))

    (define (set-light-value! light val) ;Gui itself?
      (let ((pair (assq light lights-list)))
        (cond ((eq? val 'Hp0) (set! val 1))
              ((eq? val 'Hp1) (set! val 0)))
        (send (car (cdr pair)) select val)))

    (define (send-light-status light . code)
      (let* ((list-box (car (cdr (assq light lights-list))))
             (selected (send list-box get-selection)))
        (if (not (null? code))
            (begin 
              ((light 'set-code!) (car code) connection)
              (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Lights " (light 'get-id) " changed") #t))
            (if (eq? selected 0) ;Green?
                (begin
                  ((light 'set-code!) 'Hp1 connection)
                  (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Lights " (light 'get-id) " changed to Green") #t))
                (begin
                  ((light 'set-code!) 'Hp0 connection)
                  (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Lights " (light 'get-id) " changed to Red") #t))))))

    (define (add-detectionblock! location)
      (let* ((detectionblock (make-nmbs-detectionblock location #f '() #f connection))
             (block-panel (new horizontal-panel% [parent detectionblock-panel]))
             (message (new message%
                           [label (~a "Detectionblock " location ": No trains detected")]
                           [parent block-panel])))
        (set! detectionblock-list (append detectionblock-list (list (list detectionblock block-panel message))))
        (send block-panel show #f)))

    (define (change-detection-block! pair val)
      (let ((block (car pair))
            (panel (car (cdr pair))))
        ((block 'set-occupied!) val)
        (if val
            (send (car (cdr (cdr pair))) set-label (~a "Detectionblock " (block 'get-id) ": Train " (block 'train)))
            (send (car (cdr (cdr pair))) set-label (~a "Detectionblock " (block 'get-id) ": No train detected")))))

    (define (add-barrier! id)
      (let* ((barrier (make-nmbs-barrier id #f connection))
             (list-box (new list-box%
                            [label (~a "Barrier " id)]
                            [parent barriers-panel]
                            [choices (list "Open" "Closed")]
                            [callback (lambda (button event)
                                        (send-barrier-status barrier button))])))
        (set! barriers-list (append barriers-list (list (list barrier list-box))))
        (send list-box select 0)
        (send list-box show #f)))

    (define (send-barrier-status barrier list-box)
      (let ((value (send list-box get-selection)))
        (if (= value 1) ;Closed?
            (begin ((barrier 'set-status!) #t) (set! value "closed"))
            (begin ((barrier 'set-status!) #f) (set! value "open")))
        (add-log! (~a (date->string (current-date) "~a %H:%M:%S") ": Barrier " (barrier 'get-id) " changed to " value) #t)))
    
    ;; Function to show all the children of a given panel
    (define (show-all-children panel)
      (let loop ((children (send panel get-children)))
        (cond
          ((null? children) 'done)
          (else
           (send (car children) show #t)
           (loop (cdr children))))))

    ;; Function to get the oppesite value; ex: 5 -> -5, -10 -> 10
    (define (oppesite-value value)
      (* -1 value))

    ;; Function to add a log to the log panel
    (define (add-log! log . show?)
      (let ((msg (new message%
                      [parent log-panel]
                      [label log])))
        (send msg show (car show?))
        (set! log-list (append log-list (list msg)))))

    ;; Function to remove a train from the list
    (define (remove-item! lst id)
      (define (iter lst new-list)
        (cond ((null? lst) new-list)
              ((eq? (caar lst) id)
               (iter (cdr lst) new-list))
              (else (iter (cdr lst) (cons (car lst) new-list)))))
      (iter lst '()))

    (define (remove-train-from-list id trains)
      (define (iter lst new-lst)
        (cond ((null? lst) new-lst)
              ((eq? ((car lst) 'get-id) id) (iter (cdr lst) new-lst))
              (else
               (iter (cdr lst) (cons (car lst) new-lst)))))
      (iter trains '()))

    ;; Function to remove a train by ID
    (define (remove-train! id)
      (remove-train-from-panel id)
      (set! train-list (remove-item! train-list id))
      (set! train-adt-list (remove-train-from-list id train-adt-list))
      (set! slider-list (remove-item! slider-list id))
      ((connection 'send-message) (list 'Infrabel 'remove-train! id))
      )

    ;; Function to get the value of a slider by ID
    (define (get-slider-value id)
      (define pair (assq id slider-list))
      (when pair
        (let ((sldr (slider pair)))
          (send sldr get-value))))

    ;; Function to set the slider value by ID
    (define (set-slider-value! id value)
      (define pair (assq id slider-list))
      (when pair
        (let ((sldr (slider pair)))
          (if (not (number? value)) ;Value can also be a procedure, for example value = oppesite-value
              (let ((val (send sldr get-value)))
                (send sldr set-value (value val)))
              (send sldr set-value value)))))

    ;; Function to remove a train from the train panel
    (define (remove-train-from-panel id)
      (let* ((train-pair (assq id train-list))
             (slider-pair (assq id slider-list))
             (sldr-panel (slider-panel slider-pair)))
        (when (and train-pair slider-pair)
          (send train-panel delete-child (remove-button train-pair))
          (send train-panel delete-child (message train-pair))
          (send sldr-panel delete-child (slider slider-pair))
          (send speed-panel delete-child sldr-panel))))

    ;; Dispatch function to handle messages
    (define (dispatch msg)
      (cond ((eq? msg 'add-train!) add-train!)
            ((eq? msg 'remove-train!) remove-train!)
            ((eq? msg 'get-slider-value-for-id) (get-slider-value))
            ((eq? msg 'add-switch!) add-switch!)
            ((eq? msg 'send-switch-value) send-switch-value)
            ((eq? msg 'add-light!) add-light!)
            ((eq? msg 'set-light-value!) set-light-value!)
            ((eq? msg 'send-light-status) send-light-status)
            ((eq? msg 'get-train-list) train-adt-list)
            ((eq? msg 'add-detectionblock!) add-detectionblock!)
            ((eq? msg 'get-detectionblock-list) detectionblock-list)
            ((eq? msg 'get-lights) (first-elements lights-list))
            ((eq? msg 'change-detection-block!) change-detection-block!)
            ((eq? msg 'add-barrier!) add-barrier!)
            ((eq? msg 'get-barriers-list) barriers-list)
            ((eq? msg 'get-detectionblocks) (first-elements detectionblock-list))
            ((eq? msg 'get-switches) (first-elements switch-list))
            ((eq? msg 'inform-detectionblocks) inform-detectionblocks)
            ((eq? msg 'change-train-position!) change-train-position!)
            ((eq? msg 'get-next-train-id) next-id)
            ((eq? msg 'set-next-train-id!) (lambda (val) (set! next-id val)))
            (else (display "Error: NMBS/Gui.rkt => ") (display msg))))

    ;Initialize the panels content
    (initialize-panels-content)
    ;; Show the frame
    (send main-frame show #t)
    ;((railway 'start-simple))
    dispatch))