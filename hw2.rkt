#lang racket

;Task 1

(define (cross-water-min-risk risks)
  (cond
    ;no steps provided
    [(null? risks) 0]
    ;1 step case - should jump over
    [(null? (cdr risks)) 0]
    ;choose to step or jump over
    [(if (< (+ (first risks) (cross-water-min-risk (cdr risks)))
            (+ (second risks) (cross-water-min-risk (cddr risks))))
     (+(first risks) (cross-water-min-risk (cdr risks)))
     (+(second risks) (cross-water-min-risk (cddr risks))))]))

;---------------------------------------------------------------------
;Task 2

;creates points for one command
(define (create-direction-steps direction steps last_position)
  (if (< steps 1)
  '()
  (cond
    ;if command equals to R -> add 1 to the first coodrinate on every step
    [(equal? direction "R")
     (cons (cons (+ (car last_position) 1) (cdr last_position))
           (create-direction-steps direction (- steps 1) (cons (+ (car last_position) 1) (cdr last_position))))]
    ;if command equals to L -> remove 1 from the first coodrinate on every step
    [(equal? direction "L")
      (cons (cons (- (car last_position) 1) (cdr last_position))
           (create-direction-steps direction (- steps 1) (cons (- (car last_position) 1) (cdr last_position))))]
    ;if command equals to D -> remove 1 from the second coodrinate on every step
    [(equal? direction "D")
      (cons (cons (car last_position) (- (cdr last_position) 1))
           (create-direction-steps direction (- steps 1) (cons (car last_position) (- (cdr last_position) 1))))]
    ;if command equals to U -> add 1 to the second coodrinate on every step
    [(equal? direction "U")
     (cons (cons (car last_position) (+ (cdr last_position) 1))
           (create-direction-steps direction (- steps 1) (cons (car last_position) (+ (cdr last_position) 1))))])))

;creates the whole list of points iterating through the commands one-by-one
(define (create-positions commands last_position)
  (if (null? commands)
      '()
      (cond
        ;if command equals to R -> invokes create-direction-steps with the direction R, deletes the command from the list of commands and counts the last position for the next step
        [(equal? (substring (car commands) 0 1) "R")
        (append (create-direction-steps (substring (car commands) 0 1) (string->number (substring (car commands) 1)) last_position)
         (create-positions (cdr commands) (cons (+ (car last_position) (string->number (substring (car commands) 1))) (cdr last_position))))]
        ;if command equals to L -> invokes create-direction-steps with the direction L, deletes the command from the list of commands and counts the last position for the next step
        [(equal? (substring (car commands) 0 1) "L")
        (append (create-direction-steps (substring (car commands) 0 1) (string->number (substring (car commands) 1)) last_position)
         (create-positions (cdr commands) (cons (- (car last_position) (string->number (substring (car commands) 1))) (cdr last_position))))]
        ;if command equals to D -> invokes create-direction-steps with the direction D, deletes the command from the list of commands and counts the last position for the next step
        [(equal? (substring (car commands) 0 1) "D")
        (append (create-direction-steps (substring (car commands) 0 1) (string->number (substring (car commands) 1)) last_position)
         (create-positions (cdr commands) (cons (car last_position) (- (cdr last_position) (string->number (substring (car commands) 1))))))]
        ;if command equals to U -> invokes create-direction-steps with the direction U, deletes the command from the list of commands and counts the last position for the next step
        [(equal? (substring (car commands) 0 1) "U")
        (append (create-direction-steps (substring (car commands) 0 1) (string->number (substring (car commands) 1)) last_position)
         (create-positions (cdr commands) (cons (car last_position) (+ (cdr last_position) (string->number (substring (car commands) 1))))))])))

;check for intersections of one positions of cable 1 with all of the positions of cable 2
(define (check-one-intersection pos1 positions2)
  (if (null? positions2)
            '()
            (if (equal? pos1 (car positions2))
                (list (+ (car pos1) (cdr pos1)))
                (check-one-intersection pos1 (cdr positions2)))))

;get a list of all intersetions
(define (get-intersections positions1 positions2)
  (if (null? positions1)
      '()
      (append (check-one-intersection (car positions1) positions2)
      (get-intersections (cdr positions1) positions2))))

;find the smallest distance
(define (find-min lst)
 (if (null? (cdr lst))
    (car lst)
    (if (< (first lst) (second lst))
    (find-min (cons (first lst) (cddr lst)))
    (find-min (cons (second lst) (cddr lst))))))

;find the smallest distance i times
(define (find-min-times lst number)
  (if (> number 1)
       (find-min-times (remove (find-min lst) lst) (- number 1))
      (find-min lst)
      ))

(define (length lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))

;positions1 - list with all the points of cable 1
;positions2 - list with all the points of cable 2
;intersection-number - which number intersection we should print
(define (check-intersection positions1 positions2 intersection-number)
  (define lst (get-intersections positions1 positions2))
  (cond
    [(< intersection-number 1) (print "Cables don't intersect this amount of times")]
    [(> intersection-number (length (get-intersections positions1 positions2))) (print "Cables don't intersect this amount of times")]
    [(> intersection-number 1) (find-min-times (get-intersections positions1 positions2) intersection-number)]
    [else (find-min(get-intersections positions1 positions2))]))
  
     
    

(define ((ith-manhattan cable1 cable2) intersection-number)
  ;splitting the string commands into lists of strings for easy access
  ;create-positions creates the lists with all of the points of a cable (I set them to start from (0,0))
  (check-intersection (create-positions (string-split cable1 ",") (cons 0 0)) (create-positions (string-split cable2 ",") (cons 0 0)) intersection-number))
