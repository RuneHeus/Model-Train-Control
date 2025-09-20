#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;               - Youri Coppens: AI Lab: VUB - 2023
;
;       tracks.rkt
;
;       Internal representation of tracks, blocks and switches
;

(require racket/class)

(provide
 ;; TRACKS
 straight-track% curved-track% switch-track%
 ;; CROSSING
 crossing%
 ;; SIGN
 sign%
 ;; BLOCKS
 block% switch%
 ;; CONNECTIONS
 invert reverse-indeces connect end start)

(define (remainder-2pi angle)
  (let ((quotient (truncate (/ angle (* 2 pi)))))
    (- angle (* quotient (* 2 pi)))))

;; **************** ;;
;;      TRACKS      ;;
;; **************** ;;

(define straight-track%
  (class object%
    (super-new)
    (init-field length)
    (define/public (build-gui-track)
      (make-object gui-straight-track% length))))

(define curved-track%
  (class object%
    (super-new)
    (init-field radius angle [inverted #f])
    (define/public (build-gui-track)
      (make-object gui-curved-track% radius angle inverted))))

(define (invert curved-track)
  (make-object curved-track%
    (get-field radius curved-track)
    (get-field angle  curved-track)
    (not (get-field inverted curved-track))))

(define switch-track%
  (class object%
    (super-new)
    (init-field tracks)))

(define (reverse-indeces switch-track)
  (make-object switch-track%
    (reverse (get-field tracks switch-track))))

;; ****************** ;;
;;      CROSSING      ;;
;; ****************** ;;

(define crossing%
  (class object%
    (super-new)
    (init-field length)
    (define/public (build-gui-crossing name . flipped)
      (make-object gui-crossing% name length flipped))))

;; ************** ;;
;;      SIGN      ;;
;; ************** ;;

(define sign%
  (class object%
    (super-new)
    (define/public (build-gui-sign name orientation)
      (make-object gui-sign% name orientation))))

;; ******************** ;;
;;      GUI-TRACKS      ;;
;; ******************** ;;

(define gui-straight-track%
  (class object%
    (super-new)
    (init-field length)
    (field [x 'uninitialized]
           [y 'uninitialized]
           [x2 'uninitialized]
           [y2 'uninitialized]
           [start-orientation 'uninitialized]
           [end-orientation 'uninitialized]
           [xm 'uninitialized]
           [ym 'uninitialized]
           [crossing #f]
           [sign #f])
    
    (define (build x y orientation)
      (values x
              y
              (remainder-2pi (+ pi orientation))
              (+ x (* length (cos orientation)))
              (- y (* length (sin orientation)))
              orientation
              (+ x (* (/ length 2) (cos orientation)))
              (- y (* (/ length 2) (sin orientation)))))
    
    (define/public (build-from-start-point xp yp orientation)
      (set!-values (x y start-orientation x2 y2 end-orientation xm ym) (build xp yp orientation))
      (when crossing
        (send crossing build-from-start-point x y start-orientation))
      (when sign
        (send sign build x y)))
    
    (define/public (build-from-end-point xp yp orientation)
      (set!-values (x2 y2 end-orientation x y start-orientation xm ym) (build xp yp orientation))
      (when crossing
        (send crossing build-from-end-point x y start-orientation))
      (when sign
        (send sign build x y)))
         
    (define/public (start-point-orientation)
      (list x y start-orientation))
    
    (define/public (end-point-orientation)
      (list x2 y2 end-orientation))
    
    (define/public (move-along-track distance from-start?)
      (if (> distance length)
          (- distance length)
          (if from-start?
              (cons (+ x (* distance (cos end-orientation)))
                    (- y (* distance (sin end-orientation))))
              (cons (+ x2 (* distance (cos start-orientation)))
                    (- y2 (* distance (sin start-orientation)))))))
    
    (define/public (get-length)
      length)

    (define/public (get-crossings)
      crossing)

    (define/public (get-signs)
      sign)
    
    (define/public (draw draw-label? draw-start? draw-end? name window dc-id pen-id)
      (send window draw-line dc-id pen-id x y x2 y2)
      (when draw-start? (send window draw-point dc-id 'black-fat x y))
      (when draw-end? (send window draw-point dc-id 'black-fat x2 y2))
      (when draw-label? (send window draw-solid-label (symbol->string name) dc-id 'mono-bold 'black 'white xm ym)))))


(define gui-curved-track%
  (class object%
    (super-new)
    (init-field radius angle inverted)
    (field [x 'uninitialized]
           [y 'uninitialized]
           [x2 'uninitialized]
           [y2 'uninitialized]
           [cx 'uninitialized]
           [cy 'uninitialized]
           [start-orientation 'uninitialized]
           [end-orientation 'uninitialized]
           [arc 'uninitialized]
           [xm 'uninitialized]
           [ym 'uninitialized])
    
    (define (build x y orientation angle)
      (let* ([cx (+ x (* radius (cos (- (/ pi 2) orientation))))]
             [cy (+ y (* radius (sin (- (/ pi 2) orientation))))]
             [end-radians   (+ (/ pi 2) orientation)]
             [start-radians (- end-radians angle)]
             [x2 (- cx (* radius (cos (+ (- (/ pi 2) orientation) angle))))]
             [y2 (- cy (* radius (sin (+ (- (/ pi 2) orientation) angle))))]
             [xm (- cx (* radius (cos (+ (- (/ pi 2) orientation) (/ angle 2)))))]
             [ym (- cy (* radius (sin (+ (- (/ pi 2) orientation) (/ angle 2)))))])
             
        (values x
                y
                (remainder-2pi (+ pi orientation))
                x2
                y2
                (remainder-2pi (+ (- orientation angle) (* 2 pi)))
                cx
                cy
                (list (- cx radius) (- cy radius) (* 2 radius) (* 2 radius) start-radians end-radians)
                xm
                ym)))
    
    (define (build-inverted x y orientation angle)
      (let* ([cx (- x (* radius (cos (- (/ pi 2) orientation))))]
             [cy (- y (* radius (sin (- (/ pi 2) orientation))))]
             [start-radians (+ (/ (* 3 pi) 2) orientation)]
             [end-radians (+ start-radians angle)]
             [x2 (+ cx (* radius (cos (+ (+ (/ (* 3 pi) 2) orientation) angle))))]
             [y2 (- cy (* radius (sin (+ (+ (/ (* 3 pi) 2) orientation) angle))))]
             [xm (+ cx (* radius (cos (+ (+ (/ (* 3 pi) 2) orientation) (/ angle 2)))))]
             [ym (- cy (* radius (sin (+ (+ (/ (* 3 pi) 2) orientation) (/ angle 2)))))])
        (values x
                y
                (remainder-2pi (+ pi orientation))
                x2
                y2
                (remainder-2pi (+ orientation angle))
                cx
                cy
                (list (- cx radius) (- cy radius) (* 2 radius) (* 2 radius) start-radians end-radians)
                xm
                ym)))
    
    (define/public (build-from-start-point xp yp orientation)
      (if (not inverted)
          (set!-values (x y start-orientation x2 y2 end-orientation cx cy arc xm ym) (build xp yp orientation angle))
          (set!-values (x y start-orientation x2 y2 end-orientation cx cy arc xm ym) (build-inverted xp yp orientation angle))))
    
    (define/public (build-from-end-point xp yp orientation)
      (if (not inverted)
          (set!-values (x2 y2 end-orientation x y start-orientation cx cy arc xm ym) (build-inverted xp yp orientation angle))
          (set!-values (x2 y2 end-orientation x y start-orientation cx cy arc xm ym) (build xp yp orientation angle))))
    
    (define/public (start-point-orientation)
      (list x y start-orientation))
    
    (define/public (end-point-orientation)
      (list x2 y2 end-orientation))
    
    (define (move-from-start distance)
      (let* ((angle (/ distance radius))
             (orientation (remainder-2pi (+ pi (if inverted end-orientation start-orientation))))
             (dx (+ cx (* radius (cos (- (+ (/ pi 2) orientation) angle)))))
             (dy (- cy (* radius (sin (- (+ (/ pi 2) orientation) angle))))))
        (cons dx dy)))
    
    (define (move-from-end distance)
      (let* ((angle (/ distance radius))
             (orientation (remainder-2pi (+ pi (if inverted start-orientation end-orientation))))
             (dx (+ cx (* radius (cos (+ (+ (/ (* 3 pi) 2) orientation) angle)))))
             (dy (- cy (* radius (sin (+ (+ (/ (* 3 pi) 2) orientation) angle))))))
        (cons dx dy)))
    
    (define/public (move-along-track distance from-start?)
      (if (> distance (* radius angle))
          (- distance (* radius angle))
          (if (xor from-start? inverted)
              (move-from-start distance)
              (move-from-end distance))))
    
    (define/public (get-length)
      (* radius angle))
    
    (define/public (get-crossings)
      #f)
    
    (define/public (get-signs)
      #f)
    
    (define/public (draw draw-label? draw-start? draw-end? name window dc-id pen-id)
      (send/apply window draw-arc (cons dc-id (cons pen-id arc)))
      (when draw-start? (send window draw-point dc-id 'black-fat x y))
      (when draw-end? (send window draw-point dc-id 'black-fat x2 y2))
      (when draw-label? (send window draw-solid-label (symbol->string name) dc-id 'mono-bold 'black 'white xm ym)))))


(define gui-section%
  (class object%
    (super-new)
    (init-field tracks)
    (field [gui-tracks (map (lambda (track)
                              (if (pair? track)
                                  (let ((gui-track (send (car track) build-gui-track))
                                        (other (cadr track)))
                                    (cond ((is-a? other crossing%)
                                           (let ((gui-crossing (send/apply other build-gui-crossing (cddr track))))
                                             (set-field! crossing gui-track gui-crossing)))
                                          ((is-a? other sign%)
                                           (let ((gui-sign (send/apply other build-gui-sign (cddr track))))
                                             (set-field! sign gui-track gui-sign)))
                                          (else (error "Unknown type of track accessory")))
                                    gui-track)
                                  (send track build-gui-track)))
                            tracks)])
    
    (define/public (build-from-start-point x y orientation)
      (let loop ((tracks gui-tracks)
                 (start-point (list x y orientation)))
        (when (not (null? tracks))
          (send/apply (car tracks) build-from-start-point start-point)
          (loop (cdr tracks) (send (car tracks) end-point-orientation)))))
    
    (define/public (build-from-end-point x y orientation)
      (let loop ((tracks (reverse gui-tracks))
                 (end-point (list x y orientation)))
        (when (not (null? tracks))
          (send/apply (car tracks) build-from-end-point end-point)
          (loop (cdr tracks) (send (car tracks) start-point-orientation)))))
    
    (define/public (start-point-orientation)
      (send (car gui-tracks) start-point-orientation))
    
    (define/public (end-point-orientation)
      (send (last gui-tracks) end-point-orientation))
    
    (define/public (move-along-track distance from-start?)
      (let loop ((tracks (if from-start? gui-tracks (reverse gui-tracks)))
                 (d distance))
        (let ((p-or-d (send (car tracks) move-along-track d from-start?)))
          (if (or (pair? p-or-d) (null? (cdr tracks)))
              p-or-d
              (loop (cdr tracks) p-or-d)))))
    
    (define/public (get-length)
      (foldl
       (lambda (track result)
         (+ result (send track get-length)))
       0
       gui-tracks))

    (define/public (get-crossings)
      (filter-map (lambda (gui-track) (send gui-track get-crossings)) gui-tracks))
    
    (define/public (get-signs)
      (filter-map (lambda (gui-track) (send gui-track get-signs)) gui-tracks))
    
    (define/public (draw draw-label? draw-start? draw-end? name window dc-id pen-id)
      (define first-track (car gui-tracks))
      (define last-track (last gui-tracks))
      (define middle-track
        (list-ref gui-tracks (quotient (length tracks) 2)))
      (for-each (lambda (track)
                  (send track draw (if (eq? track middle-track) draw-label? #f)
                        (if (eq? track first-track) draw-start? #f)
                        (if (eq? track last-track) draw-end? #f)
                        name window dc-id pen-id))
                gui-tracks))))


;; ********************** ;;
;;      GUI-CROSSING      ;;
;; ********************** ;;

(define gui-crossing%
  (class object%
    (super-new)
    (init-field name length flipped)
    (field [x 'uninitialized]
           [y 'uninitialized]
           [x2 'uninitialized]
           [y2 'uninitialized]
           [xl 'uninitialized]
           [yl 'uninitialized]
           [orientation 'uninitialized])

    (define time 0)
    (define action 'none)
    (define state 'open)
    
    (define (build x y xlabel ylabel orientation)
      (values x
              y
              (+ x (* length (cos orientation)))
              (- y (* length (sin orientation)))
              orientation
              (+ xlabel (* (/ length 2) (cos orientation)))
              (- ylabel (* (/ length 2) (sin orientation)))))
    
    (define/public (build-from-start-point xp yp start-orientation)
      (let ((plus (if (null? flipped) + -))
            (minus (if (null? flipped) - +)))
        (set!-values (x y x2 y2 orientation xl yl) (build (plus xp (* 25 (sin start-orientation))) (minus yp (* 25 (cos start-orientation))) (plus xp (* 50 (sin start-orientation))) (minus yp (* 50 (cos start-orientation))) start-orientation))))
    
    (define/public (build-from-end-point xp yp end-orientation)
      (let ((plus (if (null? flipped) + -))
            (minus (if (null? flipped) - +)))
        (set!-values (x2 y2 x y orientation xl yl) (build (plus xp (* 25 (sin end-orientation))) (minus yp (* 25 (cos end-orientation))) (plus xp (* 50 (sin end-orientation))) (minus yp (* 50 (cos end-orientation))) end-orientation)))
      (set! orientation (remainder-2pi (+ pi orientation))))

    (define/public (get-name)
      name)
    
    (define/public (open)
      (when (and (eq? action 'none) (eq? state 'closed))
        (set! time (current-inexact-milliseconds))
        (set! action 'open)))

    (define/public (close)
      (when (and (eq? action 'none) (eq? state 'open))
        (set! time (current-inexact-milliseconds))
        (set! action 'close)))
    
    (define/public (draw window dc-id)
      (define (draw-open-or-closed)
        (if (eq? state 'closed)
            (send window draw-line dc-id 'black x y x2 y2)
            (send window draw-point dc-id 'black x2 y2)))
      (if (eq? action 'none)
          (draw-open-or-closed)
          (let ((t (current-inexact-milliseconds)))
            (if (> (- t time) 6000)
                (begin
                  (set! state (if (eq? action 'close) 'closed 'open))
                  (set! action 'none)
                  (draw-open-or-closed))
                (if (eq? action 'close)
                    (send window draw-line dc-id 'black (+ x (* length (- 1 (/ (- t time) 6000)) (cos orientation))) (- y (* length (- 1 (/ (- t time) 6000)) (sin orientation))) x2 y2)
                    (send window draw-line dc-id 'black (+ x (* length (/ (- t time) 6000) (cos orientation))) (- y (* length (/ (- t time) 6000) (sin orientation))) x2 y2)))))
      
      (send window draw-solid-label (symbol->string name) dc-id 'mono-bold 'black 'white xl yl))))

;; ****************** ;;
;;      GUI-SIGN      ;;
;; ****************** ;;

(define gui-sign%
  (class object%
    (super-new)
    (init-field name orientation)
    (field [x 'uninitialized]
           [y 'uninitialized]
           [x8 'uninitialized]
           [y8 'uninitialized]
           [xr 'uninitialized]
           [yr 'uninitialized]
           [xg 'uninitialized]
           [yg 'uninitialized]
           [xy 'uninitialized]
           [yy 'uninitialized]
           [xw1 'uninitialized]
           [yw1 'uninitialized]
           [xw2 'uninitialized]
           [yw2 'uninitialized]
           [xw3 'uninitialized]
           [yw3 'uninitialized]
           [x6 'uninitialized]
           [y6 'uninitialized]
           [xl 'uninitialized]
           [yl 'uninitialized]
           [code '(3)]) ; Default is Hp1 (green)

    (define width 50)
    (define height 80)
    (define offset 40)

    (define Hp0 '(2))                    ;; Hp0           - RED
    (define Hp1 '(3))                    ;; Hp1           - GREEN
    (define Hp0+Sh0 '(1 3))              ;; Hp0+Sh0       - WHITE, GREEN
    (define Ks1+Zs3 '(3 7))              ;; Ks1+Zs3       - GREEN, 8
    (define Ks2 '(4 6))                  ;; Ks2           - YELLOW, WHITE
    (define Ks2+Zs3 '(4 6 7))            ;; Ks2+Zs3       - YELLOW, WHITE, 8
    (define Sh1 '(5 6))                  ;; Sh1           - WHITE
    (define Ks1+Zs3+Zs3v '(0 3 5 7))     ;; Ks1+Zs3+Zs3v  - GREEN, WHITE, 8, 6
    
    (define/public (build xp yp)
      (set! x (case orientation
                [(left) (- xp offset width)]
                [(right) (+ xp offset)]
                [(top bottom) xp]))
      (set! y (case orientation
                [(left right) yp]
                [(top) (- yp offset height)]
                [(bottom) (+ yp offset)]))
      (set! x8 (+ x 5))
      (set! y8 (- y 15))
      (set! xr (+ x (/ width 2)))
      (set! yr (+ y (/ height 4)))
      (set! xg (+ x (/ width 3)))
      (set! yg (+ y (/ height 2)))
      (set! xy (+ x (* (/ width 3) 2)))
      (set! yy (+ y (/ height 2)))
      (set! xw1 (+ x (/ width 5)))
      (set! yw1 (+ y (/ height 5)))
      (set! xw2 (+ x (/ width 2)))
      (set! yw2 (+ y (* (/ height 4) 3)))
      (set! xw3 (+ x (/ width 5)))
      (set! yw3 (+ y (* (/ height 5) 4)))
      (set! x6 (- x 10))
      (set! y6 (+ y height 15))
      (set! xl (+ x width 30))
      (set! yl (+ y (/ height 2))))
      

    (define/public (get-name)
      name)

    (define/public (set-code codep)
      (set! code
            (case codep
              [(Hp0) Hp0]
              [(Hp1) Hp1]
              [(Hp0+Sh0) Hp0+Sh0]
              [(Ks1+Zs3) Ks1+Zs3]
              [(Ks2) Ks2]
              [(Ks2+Zs3) Ks2+Zs3]
              [(Sh1) Sh1]
              [(Ks1+Zs3+Zs3v) Ks1+Zs3+Zs3v]
              (else (error "Unknow signal code")))))
    

    (define/public (draw window dc-id)
      (send window draw-rectangle dc-id 'black-thin x y width height)
      (define lambdas
        (vector
         (lambda () (send window draw-label "6" dc-id 'mono-bold 'black x6 y6))      ; SIX    0
         (lambda () (send window draw-point dc-id 'gray-thin xw1 yw1))               ; WHITE  1
         (lambda () (send window draw-point dc-id 'red xr yr))                       ; RED    2
         (lambda () (send window draw-point dc-id 'green xg yg))                     ; GREEN  3
         (lambda () (send window draw-point dc-id 'yellow xy yy))                    ; YELLOW 4
         (lambda () (send window draw-point dc-id 'gray-thin xw2 yw2))               ; WHITE  5
         (lambda () (send window draw-point dc-id 'gray-thin xw3 yw3))               ; WHITE  6
         (lambda () (send window draw-label "8" dc-id 'mono-bold 'black x8 y8))))    ; EIGHT  7
      (for-each (lambda (idx) ((vector-ref lambdas idx))) code)
      (send window draw-solid-label (symbol->string name) dc-id 'mono-bold 'black 'white xl yl))))

;; ****************** ;;
;;      SEGMENTS      ;;
;; ****************** ;;

(define block%
  (class object%
    (super-new)
    (init-field id tracks)
    (field [start-connection 'uninitialized]
           [end-connection 'uninitialized]
           [color 'black])
    
    (define gui-track
      (if (pair? tracks)
          (make-object gui-section% tracks)
          (send tracks build-gui-track)))
    
    (define visited #f)
    
    (define/public (build-gui-track from x y orientation)
      (when (not visited)
        (set! visited #t)
        (when (eq? from start-connection)
          (send gui-track build-from-start-point x y orientation)
          (when (not (eq? end-connection 'uninitialized))
            (send/apply end-connection build-gui-track (cons this (send gui-track end-point-orientation)))))
        (when (eq? from end-connection)
          (send gui-track build-from-end-point x y orientation)
          (when (not (eq? start-connection 'uninitialized))
            (send/apply start-connection build-gui-track (cons this (send gui-track start-point-orientation)))))))
    
    (define/public (initiate-build x y orientation)
      (set! visited #t)
      (send gui-track build-from-start-point x y orientation)
      (when (not (eq? end-connection 'uninitialized))
        (send/apply end-connection build-gui-track (cons this (send gui-track end-point-orientation))))
      (when (not (eq? start-connection 'uninitialized))
        (send/apply start-connection build-gui-track (cons this (send gui-track start-point-orientation)))))
    
    (define/public (start-connector)
      (cons this
            (lambda (track)
              (set! start-connection track))))
    
    (define/public (end-connector)
      (cons this
            (lambda (track)
              (set! end-connection track))))
    
    (define/public (move-along-track from distance)
      (if (eq? from start-connection)
          (let ((p-or-d (send gui-track move-along-track distance #t)))
            (if (pair? p-or-d)
                (values this from distance p-or-d)
                (if (not (eq? end-connection 'uninitialized))
                    (send end-connection move-along-track this p-or-d)
                    (error "Train Crash"))))
          (let ((p-or-d (send gui-track move-along-track distance #f)))
            (if (pair? p-or-d)
                (values this from distance p-or-d)
                (if (not (eq? start-connection 'uninitialized))
                    (send start-connection move-along-track this p-or-d)
                    (error "Train Crash"))))))
    
    (define/public (get-next-track from)
      (if (eq? from start-connection)
          end-connection
          start-connection))
    
    (define/public (get-length from)
      (send gui-track get-length))
    
    (define/public (set-color new-color)
      (set! color new-color))

    (define/public (get-crossings)
      (send gui-track get-crossings))

    (define/public (get-signs)
      (send gui-track get-signs))
    
    (define/public (draw window dc-id)
      (send gui-track draw #t #t #t id window dc-id color))))

(define (index-of el v)
  (let loop ((i 0))
    (cond ((>= i (vector-length v)) #f)
          ((eq? el (vector-ref v i)) i)
          (else (loop (+ i 1))))))


(define switch%
  (class object%
    (super-new)
    (init-field id switch-track)
    (field [start-connection 'uninitialized]
           [end-connections  (make-vector (length (get-field tracks switch-track)) 'uninitialized)]
           [active-color 'green]
           [inactive-color 'red])
    
    (define switch-position 0)
    
    (define gui-tracks
      (map (lambda (tracks)
             (if (pair? tracks)
                 (make-object gui-section% tracks)
                 (send tracks build-gui-track)))
           (get-field tracks switch-track)))
    
    (define visited #f)
    
    (define/public (build-gui-track from x y orientation)
      (when (not visited)
        (set! visited #t)
        (if (eq? from start-connection)
            (let loop ((tracks gui-tracks)
                       (i 0))
              (when (not (null? tracks))
                (let ((track (car tracks))
                      (end-connection (vector-ref end-connections i)))
                  (send track build-from-start-point x y orientation)
                  (when (not (eq? end-connection 'uninitialized))
                    (send/apply end-connection build-gui-track (cons this (send track end-point-orientation)))))
                (loop (cdr tracks) (+ i 1))))
            (let* ((i (index-of from end-connections))
                   (track (list-ref gui-tracks i)))
              (send track build-from-end-point x y orientation)
              (let* ((start-point-orientation (send track start-point-orientation))
                     (inverse-start-point-orientation (list (car start-point-orientation) (cadr start-point-orientation) (+ pi (caddr start-point-orientation)))))
                (when (not (eq? start-connection 'uninitialized))
                  (send/apply start-connection build-gui-track (cons this start-point-orientation)))
                (let loop ((tracks gui-tracks)
                           (j 0))
                  (when (not (null? tracks))
                    (when (not (= i j))
                      (let ((track (car tracks))
                            (end-connection (vector-ref end-connections j)))
                        (send/apply track build-from-start-point inverse-start-point-orientation)
                        (when (not (eq? end-connection 'uninitialized))
                          (send/apply end-connection build-gui-track (cons this (send track end-point-orientation))))))
                    (loop (cdr tracks) (+ j 1)))))))))
    
    (define/public (start-connector)
      (cons this
            (lambda (track)
              (set! start-connection track))))
    
    (define/public (end-connector track-index)
      (cons this
            (lambda (track)
              (vector-set! end-connections track-index track))))
    
    (define/public (move-along-track from distance)
      (if (eq? from start-connection)
          (let* ((track (list-ref gui-tracks switch-position))
                 (end-connection (vector-ref end-connections switch-position))
                 (p-or-d (send track move-along-track distance #t)))
            (if (pair? p-or-d)
                (values this from distance p-or-d)
                (if (not (eq? end-connection 'uninitialized))
                    (send end-connection move-along-track this p-or-d)
                    (error "Train Crash"))))
          (let* ((i (index-of from end-connections))
                 (track (list-ref gui-tracks i))
                 (p-or-d (send track move-along-track distance #f)))
            (if (pair? p-or-d)
                (values this from distance p-or-d)
                (if (not (eq? start-connection 'uninitialized))
                    (send start-connection move-along-track this p-or-d)
                    (error "Train Crash"))))))
    
    (define/public (get-next-track from)
      (if (eq? from start-connection)
          (vector-ref end-connections switch-position)
          start-connection))
    
    (define/public (get-length from)
      (if (eq? from start-connection)
          (send (list-ref gui-tracks switch-position) get-length)
          (let* ((i (index-of from end-connections))
                 (track (list-ref gui-tracks i)))
            (send track get-length))))
    
    (define/public (set-switch-position position)
      (set! switch-position position))
    
    (define/public (get-switch-position)
      switch-position)
    
    (define/public (set-colors active inactive)
      (set! active-color active)
      (set! inactive-color inactive))

    (define/public (get-crossings)
      #f)

    (define/public (get-signs)
      #f)
    
    (define/public (draw window dc-id)
      (let loop ((l gui-tracks)
                 (i 0))
        (if (null? (cdr l))
            (send (car l) draw #t #t #t id window dc-id (if (= i switch-position) ; draw label on the last gui-track
                                                            active-color
                                                            inactive-color))
            (begin
              (send (car l) draw #f #t #t id window dc-id (if (= i switch-position)
                                                              active-color
                                                              inactive-color))
              (loop (cdr l) (+ i 1))))))))

(define (start block)
  (send block start-connector))

(define (end block . index)
  (send/apply block end-connector index))

(define (connect connector-1 connector-2)
  (let ((track-1 (car connector-1))
        (track-2 (car connector-2)))
    ((cdr connector-1) track-2)
    ((cdr connector-2) track-1)))

