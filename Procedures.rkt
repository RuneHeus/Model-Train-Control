#lang racket

(provide first-elements
         list-equal?
         subset?-procedure
         delete
         print-all
         search-adt
         remove-duplicates-procedure
         remove-connection
         flatten-procedure
         get-last-item
         save-current-scenario
         read-data
         get-data-by-category
         return-train-adt-list)


(define (first-elements lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (cons (car (car lst)) (first-elements (cdr lst))))
        (else (cons (car lst) (first-elements (cdr lst))))))

(define (list-equal? lst1 lst2)
  (and (subset? lst1 lst2)
       (subset? lst2 lst1)))

(define (subset?-procedure lst1 lst2)
  (cond
    ((null? lst1) #t)
    ((member (car lst1) lst2)
     (subset? (cdr lst1) (delete (car lst1) lst2)))
    (else #f)))

(define (delete item lst)
  (cond
    ((null? lst) '())
    ((equal? item (car lst)) (cdr lst))
    (else (cons (car lst) (delete item (cdr lst))))))

(define (print-all lst)
  (cond ((null? lst) '())
        (else
         (display (car lst))
         (newline)
         (print-all (cdr lst)))))

(define (search-adt id lst1)
  (define (iter lst)
    (cond ((null? lst) #f)
          ((eq? ((car lst) 'get-id) id) (car lst))
          (else (iter (cdr lst)))))
  (iter lst1))

(define (remove-duplicates-procedure lst)
  (define (remove-helper lst seen)
    (cond ((null? lst) '())
          ((member (car lst) seen)
           (remove-helper (cdr lst) seen))
          (else
           (cons (car lst) (remove-helper (cdr lst) (cons (car lst) seen))))))
  (remove-helper lst '()))

(define (remove-connection connection connections)
  (filter (lambda (c)
            (not (and (equal? (car c) (car connection))
                      (equal? (cdr c) (cdr connection)))))
          connections))

(define (flatten-procedure lst)
  (cond ((null? lst) '())           
        ((not (pair? lst)) (list lst)) 
        (else (append (flatten (car lst)) (flatten (cdr lst))))))
(define (get-last-item lst)
  (cond ((null? (cdr lst)) (car lst)) 
        (else (get-last-item (cdr lst)))))

(define (string->boolean str)
  (cond
    ((string=? str "#t") #t)
    ((string=? str "#f") #f)
    (else (error "Invalid boolean string representation"))))

(define (string->list str)
  (if (string=? str "()")
      '()
      (let ((cleaned-str (string-trim str "()")))
        (read (open-input-string cleaned-str)))))

(define (get-data-by-category category data-list)
  (define (find-category category data-list)
    (cond
      ((null? data-list) #f)
      ((eq? category (caar data-list)) (cadar data-list))
      (else (find-category category (cdr data-list)))))
  (find-category category data-list))

(define (return-train-adt-list lst train-adt-list) ;This will return a list of the Train ADT's,the given argument is a list of adt id's
  (let ((result '()))
    (define (iter lijst)
      (cond ((null? lijst) result)
            (else
             (set! result (append result (list (search-adt (car lijst) train-adt-list))))
             (iter (cdr lijst)))))
    (iter lst)))

;Everything file related
(define (save-current-scenario trains lights barriers switches blocks filename)

  (displayln "Saving current scenario")
  
  (define (save-trains trains output-port)
    (displayln "Trains" output-port) ;Writing the Trains title to the output file
    (for-each (lambda (train)
                (let ((train-string (format "~a ~a ~a ~a" (train 'get-id) (train 'get-pos) (train 'get-speed) (train 'get-destination))))
                  (displayln train-string output-port)))
              trains))
  
  (define (save-lights lights output-port)
    (displayln "Lights" output-port)
    (for-each (lambda (light)
                (let ((light-string (format "~a ~a" (light 'get-id) (light 'get-code))))
                  (displayln light-string output-port)))
              lights))
  
  (define (save-barriers barriers output-port)
    (displayln "Barriers" output-port)
    (for-each (lambda (barrier)
                (display "Barrier closed?: ")
                (displayln (barrier 'closed?))
                (let ((barrier-string (format "~a ~a" (barrier 'get-id) (barrier 'closed?))))
                  (displayln barrier-string output-port)))
              barriers))
  
  (define (save-switches switches output-port)
    (displayln "Switches" output-port)
    (for-each (lambda (switch)
                (let ((switch-string (format "~a ~a ~a ~a" (switch 'get-id) (switch 'in) (switch 'out1) (switch 'out2))))
                  (displayln switch-string output-port)))
              switches))
  
  (define (save-blocks blocks output-port)
    (displayln "Blocks" output-port)
    (for-each (lambda (block)
                (let ((block-string (format "~a ~a ~a ~a" (block 'get-id) (block 'train) (map (lambda (train)
                                                                                                (if train
                                                                                                    (train 'get-id)
                                                                                                    #f)) (block 'next-trains)) (block 'occupied?))))
                  (displayln block-string output-port)))
              blocks))

  (call-with-output-file filename
    (lambda (output-port)
      (save-trains trains output-port)
      (displayln "" output-port)
      (save-lights lights output-port)
      (displayln "" output-port)
      (save-barriers barriers output-port)
      (displayln "" output-port)
      (save-switches switches output-port)
      (displayln "" output-port)
      (save-blocks blocks output-port))
    #:exists 'truncate
    #:mode 'text))

(define (read-data filename)
  (define train-data '())
  (define light-data '())
  (define barrier-data '())
  (define switch-data '())
  (define block-data '())
  
  (call-with-input-file filename
    (lambda (input-port)
      (let title-loop ()
        (let ((first-line (read-line input-port)))
          (cond
            ((eof-object? first-line)
             (close-input-port input-port)
             (list train-data light-data barrier-data switch-data block-data))
            ((string-contains? first-line "Trains")
             (let loop ()
               (let ((line (read-line input-port)))
                 (cond
                   ((not (eof-object? line))
                    (let ((parts (string-split line)))
                      (if (null? parts)
                          (begin (set! train-data (list 'Trains train-data))
                                 (title-loop))
                          (let ((id (string->symbol (list-ref parts 0)))
                                (block (string->symbol (list-ref parts 1)))
                                (speed (string->number (list-ref parts 2)))
                                (destination (string->symbol (list-ref parts 3))))
                            (set! train-data (append train-data (list (list id block speed destination))))
                            (loop)))))
                   (else
                    (set! train-data (cons 'Trains train-data))
                    (title-loop))))))
            ((string-contains? first-line "Lights")
             (let loop ()
               (let ((line (read-line input-port)))
                 (cond
                   ((not (eof-object? line))
                    (let ((parts (string-split line)))
                      (if (null? parts)
                          (begin (set! light-data (list 'Lights light-data))
                                 (title-loop))
                          (let ((id (string->symbol (list-ref parts 0)))
                                (code (string->symbol (list-ref parts 1))))
                            (set! light-data (append light-data (list (list id code))))
                            (loop)))))
                   (else
                    (set! light-data (cons 'Switches light-data))
                    (title-loop))))))
            ((string-contains? first-line "Barriers")
             (let loop ()
               (let ((line (read-line input-port)))
                 (cond
                   ((not (eof-object? line))
                    (let ((parts (string-split line)))
                      (if (null? parts)
                          (begin (set! barrier-data (list 'Barriers barrier-data))
                                 (title-loop))
                          (let ((id (string->symbol (list-ref parts 0)))
                                (code (string->boolean (list-ref parts 1))))
                            (set! barrier-data (append barrier-data (list (list id code))))
                            (loop)))))
                   (else
                    (set! barrier-data (cons 'Barriers barrier-data))
                    (title-loop))))))
            ((string-contains? first-line "Switches")
             (let loop ()
               (let ((line (read-line input-port)))
                 (cond
                   ((not (eof-object? line))
                    (let ((parts (string-split line)))
                      (if (null? parts)
                          (begin (set! switch-data (list 'Switches switch-data))
                                 (title-loop))
                          (let ((id (string->symbol (list-ref parts 0)))
                                (in (string->symbol (list-ref parts 1)))
                                (out1 (string->symbol (list-ref parts 2)))
                                (out2 (string->symbol (list-ref parts 3))))
                            (set! switch-data (append switch-data (list (list id in out1 out2))))
                            (loop)))))
                   (else
                    (set! switch-data (cons 'Switches switch-data))
                    (title-loop))))))
            ((string-contains? first-line "Blocks")
             (let loop ()
               (let ((line (read-line input-port)))
                 (cond
                   ((not (eof-object? line))
                    (let ((parts (string-split line)))
                      (if (null? parts)
                          (begin 
                            (set! block-data (cons 'Blocks block-data))
                            (title-loop))
                          (let ((id (string->symbol (list-ref parts 0)))
                                (train (string->number (list-ref parts 1)))
                                (next-trains (string->list (list-ref parts 2)))
                                (occupied? (string->boolean (list-ref parts 3))))
                            (set! block-data (append block-data (list (list id train next-trains occupied?))))
                            (loop)))))
                   (else
                    (set! block-data (list 'Blocks block-data))
                    (title-loop))))))
            (else
             (displayln light-data)
             (close-input-port input-port)
             (list train-data light-data barrier-data switch-data block-data))))))))