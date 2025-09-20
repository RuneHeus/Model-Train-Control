#lang racket

;
; GUI Simulator - Joeri De Koster: SOFT: VUB - 2017
;               - Youri Coppens: AI LAB: VUB - 2023
;
;       graphics.rkt
;
;       create window with a multi-layered canvas

(require racket/gui/base)
(require racket/class)
(require racket/dict)

(provide window%)

(define augmented-frame%
  (class frame%
    (init-field title width height [close-callback (thunk (void))])
    (super-new [label title]
               [width width]
               [height height])
    
    (define/augment (on-close) ; Perform some functionality after clicking the window’s close box
      (close-callback))))
    

(define window%
  (class object%
    (super-new)
    (init-field title width height [close-callback (thunk (void))])
  
    (define (scale value)
      (* value 0.4))

    (set! width (exact-round (scale width)))
    (set! height (exact-round (scale height)))

    (define frame (make-object augmented-frame% title width height close-callback))

    ; Make the drawing area
    (define canvas (new canvas% [parent frame]
                        [style '(no-focus)]))
    
    ; Get the canvas's drawing context
    (define dc (send canvas get-dc))
    
    ;brushes dict
    (define brushes (make-hash))
    (define (add-brush id brush)
      (dict-set! brushes id brush))
    (define (get-brush id)
      (dict-ref brushes id #f))
    
    ;pens dict
    (define pens (make-hash))
    (define (add-pen id pen)
      (dict-set! pens id pen))
    (define (get-pen id)
      (dict-ref pens id #f))

    ;fonts dict
    (define fonts (make-hash))
    (define (add-font id font)
      (dict-set! fonts id font))
    (define (get-font id)
      (dict-ref fonts id #f))

    ;color dict
    (define colors (make-hash))
    (define (add-color id color)
      (dict-set! colors id color))
    (define (get-color id)
      (dict-ref colors id #f))
    
    ; Make some pens, brushes, fonts and colors
    (add-color 'red (make-object color% 230 0 0))
    (add-color 'yellow (make-object color% 204 204 0))
    (add-color 'orange (make-object color% 255 128 0))
    (add-color 'green (make-object color% 0 230 0))
    (add-color 'blue (make-object color% 0 0 230))
    (add-color 'purple (make-object color% 128 11 128))
    (add-color 'black (make-object color% 0 0 0))
    (add-color 'gray (make-object color% 128 128 128))
    (add-color 'white (make-object color% 255 255 255))
    
    (add-pen 'red (make-object pen% (get-color 'red) (scale 15) 'solid))
    (add-pen 'yellow (make-object pen% (get-color 'yellow) (scale 15) 'solid))
    (add-pen 'yellow-fat (make-object pen% (get-color 'yellow) (scale 45) 'solid))
    (add-pen 'orange (make-object pen% (get-color 'orange) (scale 15) 'solid))
    (add-pen 'orange-fat (make-object pen% (get-color 'orange) (scale 45) 'solid))
    (add-pen 'green (make-object pen% (get-color 'green) (scale 15) 'solid))
    (add-pen 'black-thin (make-object pen% (get-color 'black) (scale 7) 'solid))
    (add-pen 'black (make-object pen% (get-color 'black) (scale 15) 'solid))
    (add-pen 'black-fat (make-object pen% (get-color 'black) (scale 24) 'solid))
    (add-pen 'blue (make-object pen% (get-color 'blue) (scale 15) 'solid))
    (add-pen 'purple (make-object pen% (get-color 'purple) (scale 15) 'solid))
    (add-pen 'gray-thin (make-object pen% (get-color 'gray) (scale 10) 'solid))
    (add-pen 'white (make-object pen% (get-color 'white) (scale 15) 'solid))
    
    (add-brush 'transparent (make-object brush% (get-color 'black) 'transparent))

    (add-font 'default (make-font))
    (add-font 'mono-bold (make-font #:family 'modern #:weight 'bold #:size (scale 24)))

    ; Bitmap dicts representing the layers of the canvas
    ; Used structure is an association list to maintain the order of layers
    (define bitmaps '())
    (define bitmap-dcs '())
    
    (define/public (create-layer id)
      (let* ((buffer-bitmap (make-object bitmap% width height #f #t))
             (buffer-bitmap-dc (make-object bitmap-dc% buffer-bitmap)))
        (send buffer-bitmap-dc set-brush (get-brush 'transparent))
        (set! bitmaps (cons (cons id buffer-bitmap) bitmaps))
        (set! bitmap-dcs (cons (cons id buffer-bitmap-dc) bitmap-dcs))))

    (define (get-bitmap id)
      (cdr (assoc id bitmaps)))
    
    (define (get-bitmap-dc id)
      (cdr (assoc id bitmap-dcs)))

    ; Draw to a specific layer
    (define/public (draw-point dc-id pen-id x y)
      (let ((dc (get-bitmap-dc dc-id))
             (pen (get-pen pen-id)))
        (send dc set-pen pen)
        (send dc draw-point (scale x) (scale y))))
    
    (define/public (draw-arc dc-id pen-id x y width height start-radians end-radians)
      (let ((dc (get-bitmap-dc dc-id)))
        (send dc set-pen (get-pen pen-id))
        (send dc draw-arc (scale x) (scale y) (scale width) (scale height) start-radians end-radians)))
    
    (define/public (draw-line dc-id pen-id x1 y1 x2 y2)
      (let ((dc (get-bitmap-dc dc-id)))
        (send dc set-pen (get-pen pen-id))
        (send dc draw-line (scale x1) (scale y1) (scale x2) (scale y2)))) 
    
    (define/public (draw-rectangle dc-id pen-id x y width height)
      (let ((dc (get-bitmap-dc dc-id)))
        (send dc set-pen (get-pen pen-id))
        (send dc draw-rectangle (scale x) (scale y) (scale width) (scale height)))) 

    (define/public (draw-label label dc-id font-id foreground-color-id x y)
      (let ([dc (get-bitmap-dc dc-id)])
        (send dc set-font (get-font font-id))
        (send dc set-text-mode 'transparent)
        (send dc set-text-foreground (get-color foreground-color-id))
        (let-values ([(w h d a) (send dc get-text-extent label)])
          (send dc draw-text label (- (scale x) (/ w 2)) (- (scale y) (/ h 2))))))

    (define/public (draw-solid-label label dc-id font-id foreground-color-id background-color-id x y)
      (let ([dc (get-bitmap-dc dc-id)])
        (send dc set-font (get-font font-id))
        (send dc set-text-mode 'solid)
        (send dc set-text-foreground (get-color foreground-color-id))
        (send dc set-text-background (get-color background-color-id))
        (let-values ([(w h d a) (send dc get-text-extent label)])
          (send dc draw-text label (- (scale x) (/ w 2)) (- (scale y) (/ h 2))))))
    
    (define/public (clear-layer dc-id)
      (send (get-bitmap-dc dc-id) clear)
      (send (get-bitmap-dc dc-id) erase))

    (define/public (clear-all-layers)
      (for-each (lambda (bitmap-dc)
                  (send bitmap-dc clear)
                  (send bitmap-dc erase))
                (map cdr bitmap-dcs)))

    (define/public (set-frame-title title)
      (send frame set-label title))
    
    (define/public (refresh)
      (send dc clear)
      (send dc erase)
      (for-each
       (lambda (buffer-bitmap)
         (send dc draw-bitmap (cdr buffer-bitmap) 0 0))
       (reverse bitmaps)))

    (define/public (suspend-flush)
      (send canvas suspend-flush))

    (define/public (resume-flush)
      (send canvas resume-flush))
    
    (define/public (flush)
      (send canvas flush))
    
    ; Show the frame
    (send frame show #t)))