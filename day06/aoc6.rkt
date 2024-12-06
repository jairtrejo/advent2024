#lang racket

(define (read-map)
  (for/fold ([guard-position #f]
             [guard-direction #f]
             [obstacles '()]
             [size (cons 0 0)])
            ([row (in-naturals)]
             [line (in-lines)]
             #:when #t
             [col (in-naturals)]
             [c line])
            (define size (cons (add1 row) (add1 col)))
            (match c
              [#\^ (values (cons row col) (cons -1 0) obstacles size)]
              [#\# (values guard-position guard-direction (cons (cons row col) obstacles) size)]
              [else (values guard-position guard-direction obstacles size)])))

(define (hits? guard-position guard-direction obstacle)
  (or (and (positive? (* (car guard-direction) (- (car obstacle) (car guard-position))))
           (equal? (cdr guard-position) (cdr obstacle)))
      (and (positive? (* (cdr guard-direction) (- (cdr obstacle) (cdr guard-position))))
           (equal? (car guard-position) (car obstacle)))))

(define (bounce guard-direction obstacle)
  (define opposite-direction ((compose turn-right turn-right) guard-direction))
  (cons (+ (car obstacle) (car opposite-direction))
        (+ (cdr obstacle) (cdr opposite-direction))))

(define (turn-right direction)
  (match direction
    [(cons -1 0) (cons 0 1)]
    [(cons 0 1) (cons 1 0)]
    [(cons 1 0) (cons 0 -1)]
    [(cons 0 -1) (cons -1 0)]))

(define (clamp v mi ma)
  (max mi (min ma v)))

(define (area-exit guard-position guard-direction size)
  (cons (clamp (+ (car guard-position) (* (car size) (car guard-direction)))
               0 (sub1 (car size)))
        (clamp (+ (cdr guard-position) (* (cdr size) (cdr guard-direction)))
               0 (sub1 (cdr size)))))

(define (find-obstacle guard-position guard-direction obstacles)
  (define sorted-obstacles
    (match guard-direction
      [(cons -1 0) (sort obstacles > #:key car)]
      [(cons 0 1) (sort obstacles < #:key cdr)]
      [(cons 1 0) (sort obstacles < #:key car)]
      [(cons 0 -1) (sort obstacles > #:key cdr)]))
  (for/first ([obstacle sorted-obstacles]
              #:when (hits? guard-position guard-direction obstacle))
             obstacle))

(define (direction waypoint-a waypoint-b)
  (cons (/ (- (car waypoint-b) (car waypoint-a))
           (max 1 (abs (- (car waypoint-b) (car waypoint-a)))))
        (/ (- (cdr waypoint-b) (cdr waypoint-a))
           (max 1 (abs (- (cdr waypoint-b) (cdr waypoint-a)))))))

(define (poses path)
  (define reversed-path (reverse path))
  (for/list ([waypoint-a reversed-path]
             [waypoint-b (rest reversed-path)])
            (cons waypoint-b (direction waypoint-a waypoint-b))))

(define (patrol guard-position guard-direction obstacles size [path '()] [path-poses '()])
  (define obstacle (find-obstacle guard-position guard-direction obstacles))
  (define new-path (cons guard-position path))
  (define new-poses (poses new-path))
  (cond
    [obstacle (define new-guard-position (bounce guard-direction obstacle))
              (define new-guard-direction (turn-right guard-direction))
              (define new-guard-pose (cons new-guard-position guard-direction))
              (if (member new-guard-pose new-poses)
                #f  
                (patrol new-guard-position
                        new-guard-direction
                        obstacles
                        size
                        new-path
                        new-poses))]
    [else (cons (area-exit guard-position guard-direction size) new-path)]))

(define (steps waypoint-a waypoint-b)
  (set-add (match (list waypoint-a waypoint-b)
             [(list (cons rowa col) (cons rowb col))
              (for/set ([row (in-range rowa rowb (if (> rowb rowa) 1 -1))])
                (cons row col))]
             [(list (cons row cola) (cons row colb))
              (for/set ([col (in-range cola colb (if (> colb cola) 1 -1))])
                (cons row col))])
           waypoint-b))

(define (distinct path)
  (for/fold ([s (set)])
            ([waypoint-a path]
             [waypoint-b (rest path)])
    (set-union s
               (steps waypoint-a waypoint-b))))

(define (part-one)
  (define-values (guard-position guard-direction obstacles size) (read-map))
  (define path (patrol guard-position guard-direction obstacles size))
  (set-count (distinct path)))

(define (turn-around d)
  (turn-right (turn-right d)))

(define (segment-obstructions waypoint-a waypoint-b obstacles)
  (define travel-direction (direction waypoint-a waypoint-b))
  (define search-direction (turn-right travel-direction))
  (define starting-points (set->list (steps waypoint-a waypoint-b)))
  (for/set ([starting-point starting-points]
            #:do ((define obstacle
                    (find-obstacle starting-point search-direction obstacles)))
            #:when obstacle
            #:do ((define obstruction (bounce (turn-around travel-direction) starting-point))))
           obstruction))

(define (part-two)
  (define-values (guard-position guard-direction obstacles size) (read-map))
  (define path (patrol guard-position guard-direction obstacles size))
  (define reversed-path (reverse path))
  (for/fold ([obstructions (set)]
             #:result  (set-count obstructions))
            ([waypoint-a reversed-path]
             [waypoint-b (rest reversed-path)])
            (define new-obstructions (segment-obstructions waypoint-a waypoint-b obstacles))
            (define new-looping-obstructions
              (for/set ([o new-obstructions]
                        #:when (and (not (member o obstacles))
                                    (not (patrol guard-position guard-direction (cons o obstacles) size))))
                       o))
            (values
              (set-union new-looping-obstructions
                         obstructions))))

(with-input-from-file "example.txt"
  part-one)

(with-input-from-file "example.txt"
  part-two)
