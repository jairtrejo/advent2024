#lang racket

(define (read-warehouse)
  (for/fold ([warehouse (hash)]
             #:result (values (set-first (hash-ref warehouse #\@))
                              (hash-remove warehouse #\@)))
            ([row (in-naturals)]
             [line (in-lines)]
             #:break (equal? line "")
             #:when #t
             [col (in-naturals)]
             [c line]
             #:unless (equal? c #\.))
            (hash-update warehouse
                         c
                         (curryr set-add (cons row col))
                         (set))))

(define/match (up pos)
  [((cons x y)) (cons (sub1 x) y)])

(define/match (left pos)
  [((cons x y)) (cons x (sub1 y))])
  
(define/match (down pos)
  [((cons x y)) (cons (add1 x) y)])
  
(define/match (right pos)
  [((cons x y)) (cons x (add1 y))])

(define (read-instructions)
  (for*/list ([line (in-lines)]
              [c line])
             (match c [#\^ up] [#\v down] [#\> right] [#\< left])))

(define-values (the-robot the-warehouse the-instructions)
  (with-input-from-file "example.txt"
    (λ ()
      (define-values (robot warehouse) (read-warehouse))
      (define instructions (read-instructions))
      (values robot warehouse instructions))))
the-robot
the-warehouse
the-instructions

(define (pushable-boxes warehouse robot direction [bs (set)])
  (define walls (hash-ref warehouse #\#))
  (define boxes (hash-ref warehouse #\O))
  (define next (direction robot))
  (cond
    [(set-member? walls next) #f]
    [(set-member? boxes next) (pushable-boxes warehouse
                                              next
                                              direction
                                              (set-add bs next))]
    [else bs]))
(pushable-boxes the-warehouse (cons 1 4) down)

(define (push-boxes warehouse robot direction)
  (define pushable (pushable-boxes warehouse robot direction))
  (define boxes (hash-ref warehouse #\O))
  (if pushable
    (hash-set warehouse #\O
      (for/set ([b boxes])
               (if (set-member? pushable b)
                   (direction b)
                   b)))
    #f))
(push-boxes the-warehouse (cons 2 2) left)

(define (score warehouse)
  (define boxes (hash-ref warehouse #\O))
  (for/sum ([b boxes])
           (+ (* 100 (car b)) (cdr b))))
(score the-warehouse)

(define (part-one)
  (define-values (robot warehouse) (read-warehouse))
  (define instructions (read-instructions))
  (define final-warehouse
    (for/fold ([robot robot]
               [warehouse warehouse]
               #:result warehouse)
              ([direction instructions])
              (define moved-robot (direction robot))
              (define moved-warehouse (push-boxes warehouse robot direction))
              (if moved-warehouse
                  (values moved-robot moved-warehouse)
                  (values robot warehouse))))
  (score final-warehouse))
  ;; (sort (set->list (hash-ref final-warehouse #\O)) < #:key car))

(with-input-from-file "input.txt" part-one)
