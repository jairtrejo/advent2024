#lang racket

(define (read-plots)
  (for/fold ([plots (hash)])
            ([row (in-naturals)]
             [line (in-lines)]
             #:when #t
             [col (in-naturals)]
             [c line])
            (hash-update plots
                         c
                         (curryr set-add (cons row col))
                         (set))))

(define (around pos)
  (match-define (cons x y) pos)
  (set (cons (sub1 x) y)
       (cons x (sub1 y))
       (cons (add1 x) y)
       (cons x (add1 y))))

(define (extract-region candidates [region (set)])
  (cond
    [(set-empty? candidates) region]
    [(set-empty? region)
     (define seed (set-first candidates))
     (extract-region (set-remove candidates seed) (set seed))]
    [else
      (define neighbors (apply set-union
                               (set-map region around)))
      (define chunk (set-intersect neighbors candidates))
      (if (set-empty? chunk)
          region
          (extract-region (set-subtract candidates chunk)
                          (set-union region chunk)))]))

(define (extract-regions candidates)
  (cond
    [(set-empty? candidates) '()]
    [else (define region (extract-region candidates))
          (cons region (extract-regions (set-subtract candidates region)))]))

(define (perimeter region)
  (for/sum ([p region])
           (set-count (set-subtract (around p)
                                    region))))

(define area set-count)

(define (part-one)
  (define plots (read-plots))
  (for*/sum ([plants (hash-values plots)]
             [region (extract-regions plants)])
            (* (area region) (perimeter region))))

(with-input-from-file "input.txt" part-one)

(define/match (up pos)
  [((cons x y)) (cons (sub1 x) y)])

(define/match (left pos)
  [((cons x y)) (cons x (sub1 y))])
  
(define/match (down pos)
  [((cons x y)) (cons (add1 x) y)])
  
(define/match (right pos)
  [((cons x y)) (cons x (add1 y))])
  
(define (sides region)
  (for/sum ([direction (list up left down right)])
    (length
      (extract-regions
        (for/set ([plot region]
                  #:unless (set-member? region
                                        (direction plot)))
                 plot)))))

(define (part-two)
  (define plots (read-plots))
  (for*/sum ([plants (hash-values plots)]
             [region (extract-regions plants)])
            (* (area region) (sides region))))

(with-input-from-file "input.txt" part-two)
