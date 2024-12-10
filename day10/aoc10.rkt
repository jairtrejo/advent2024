#lang racket

(define (read-heights)
  (for/fold ([heights (hash)])
            ([row (in-naturals)]
             [line (in-lines)]
             #:when #t
             [col (in-naturals)]
             [c line])
            (values (hash-update heights
                                 (string->number (string c))
                                 (curryr set-add (cons row col))
                                 (set)))))

(define (around pos)
  (match-define (cons x y) pos)
  (set (cons (sub1 x) y)
       (cons x (sub1 y))
       (cons (add1 x) y)
       (cons x (add1 y))))

(define (summits trailhead heights)
  (for/fold ([reachable (set trailhead)])
            ([h 9])
            (define surroundings (for/fold ([s (set)])
                                           ([r reachable])
                                           (set-union s (around r))))
            (set-intersect surroundings (hash-ref heights (add1 h)))))

(define (part-one)
  (define heights (read-heights))
  (for/sum ([trailhead (hash-ref heights 0)])
           (set-count (summits trailhead heights))))

(with-input-from-file "input.txt" part-one)

(define (trails trailhead height heights)
  (define next-steps (set-intersect (around trailhead)
                                    (hash-ref heights (add1 height) (set))))
  (cond
    [(= height 9) 1]
    [(set-empty? next-steps) 0]
    [else (for/sum ([s next-steps])
                   (trails s (add1 height) heights))]))

(define (part-two)
  (define heights (read-heights))
  (for/sum ([trailhead (hash-ref heights 0)])
           (trails trailhead 0 heights)))

(with-input-from-file "input.txt" part-two)
