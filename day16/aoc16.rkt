#lang racket

(require racket/hash)

(define (read-maze)
  (for/fold ([maze (hash)]
             #:result (values (set-first (hash-ref maze #\S)) 
                              (set-first (hash-ref maze #\E))
                              maze))
            ([row (in-naturals)]
             [line (in-lines)]
             #:when #t
             [col (in-naturals)]
             [c line])
            (hash-update maze
                         c
                         (curryr set-add (cons row col))
                         (set))))
(define-values (the-start the-end the-maze)
  (with-input-from-file "example.txt" read-maze))
the-start
the-end
the-maze

(define/match (north pos)
  [((cons x y)) (cons (sub1 x) y)])

(define/match (west pos)
  [((cons x y)) (cons x (sub1 y))])
  
(define/match (south pos)
  [((cons x y)) (cons (add1 x) y)])
  
(define/match (east pos)
  [((cons x y)) (cons x (add1 y))])

(define (clockwise direction)
  (cond
    [(equal? direction north) east]
    [(equal? direction east) south]
    [(equal? direction south) west]
    [(equal? direction west) north]))

(define counter-clockwise
  (compose clockwise clockwise clockwise))

(define directions
  (list north east south west))

(define (graph maze)
  (define corridors (set-union (hash-ref maze #\S)
                               (hash-ref maze #\.)))
  (define end (set-first (hash-ref maze #\E)))
  (hash-union
    (for*/hash ([c corridors]
                [d directions]
                #:when (set-member? corridors (d c)))
               (define from (cons c d))
               (define to (cons (d c) d))
               (values (cons from to) 1))
    (for*/hash ([c corridors]
                [d directions]
                #:when (equal? end (d c))
                [d2 directions])
               (define from (cons c d))
               (define to (cons end d2))
               (values (cons from to) 1))
    (for*/hash ([c corridors]
                [d directions]
                [t (list (clockwise d)
                         (counter-clockwise d))])
               (define from (cons c d))
               (define to (cons c t))
               (values (cons from to) 1000))))
(define the-graph (graph the-maze))

the-graph

(hash-ref the-graph (cons (cons (cons 1 14) east)
                          (cons the-end south)))

(define (neighbors graph node)
  (match-define (cons pos dir) node)
  (define candidates
    (list (cons pos (clockwise dir))
          (cons pos (counter-clockwise dir))
          (cons (dir pos) dir)))
  (filter (λ (n) (hash-has-key? graph
                                (cons (cons pos dir) n)))
          candidates))
(neighbors the-graph (cons (cons 3 5) south))

(define (h-distance from to)
  (match-define (cons pfrom dfrom) from)
  (match-define (cons pto dto) to)
  (+ (abs (- (car pfrom) (car pto)))
     (abs (- (cdr pfrom) (cdr pto)))))
(h-distance (cons (cons 1 0) east) (cons (cons 0 0) north))

(define (insert queue node priority priorities)
  (define-values (before after)
    (splitf-at queue (λ (e) (> priority (hash-ref priorities e)))))
  (append before (cons node after)))
(insert '(a b c d) 'x 2 (hash 'a 1 'b 2 'c 3 'd 4))

(length (hash-keys the-graph))

(define (a-star graph start end [frontier #f] [gscore #f] [fscore #f] [came-from #f])
  (cond
    [(not frontier)
     (a-star graph start end
             (list start)
             (hash start 0)
             (hash start (h-distance start end))
             (hash))]
    [(empty? frontier) #f]
    [else 
      (define current (first frontier))
      (cond
        [(equal? (car current) (car end)) (hash-ref gscore current)]
        [else
          (define ns (neighbors graph current))
          (define-values (u-frontier u-gscore u-fscore)
            (for/fold ([u-frontier (rest frontier)]
                       [u-gscore gscore]
                       [u-fscore fscore])
                      ([n ns]
                       #:do ((define t-gscore
                               (+ (hash-ref u-gscore current +inf.0)
                                  (hash-ref graph (cons current n)))))
                       #:when (< t-gscore (hash-ref u-gscore n +inf.0)))
                      (define t-fscore (+ t-gscore (h-distance n end)))
                      (values (insert u-frontier n t-fscore u-fscore)
                              (hash-set u-gscore n t-gscore)
                              (hash-set u-fscore n t-fscore))))
          (a-star graph start end u-frontier u-gscore u-fscore)])]))
(a-star the-graph (cons the-start east) (cons the-end west))

(define (part-one)
  (define-values (start end maze) (read-maze))
  (define maze-graph (graph maze))
  (a-star maze-graph (cons start east) (cons end #f)))
(with-input-from-file "example.txt" part-one)

(with-input-from-file "input.txt" part-one)
