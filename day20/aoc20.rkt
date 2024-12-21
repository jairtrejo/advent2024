#lang racket

(define (read-course)
  (for/fold ([course (hash)]
             #:result (let*
                        ([start (set-first (hash-ref course #\S))] 
                         [end (set-first (hash-ref course #\E))]
                         [corridors (set-union (hash-ref course #\.)
                                               (set start end))]
                         [walls (hash-ref course #\#)])
                       (values start
                               end
                               corridors
                               walls)))
            ([row (in-naturals)]
             [line (in-lines)]
             #:when #t
             [col (in-naturals)]
             [c line])
            (hash-update course
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

(define directions
  (list up right down left))

(define (graph corridors walls)
  (for*/hash ([from corridors]
              [dir directions]
              #:do ((define to (dir from)))
              #:unless (set-member? walls to))
             (values (cons from to) 1)))

(define (neighbors node)
  (for/set ([d directions])
           (d node)))

(define (all-nodes g)
  (for/set ([k (hash-keys g)]
            #:do ((match-define (cons from to) k))
            [node (list from to)])
           node))

(define (insert queue node priority priorities)
  (define-values (before after)
    (splitf-at queue (λ (e) (> priority (hash-ref priorities e +inf.0)))))
  (append before (cons node after)))

(define (dijkstra g start [visited #f] [distances #f])
  (cond
    [(not visited)
     (define distances (hash start 0))
     (define visited (cons start
                           (set->list (set-remove (all-nodes g)
                                                  start))))
     (dijkstra g start visited distances)]
    [(empty? visited) distances]
    [else
      (define current (first visited))
      (define current-distance (hash-ref distances current))
      (define ns (neighbors current))
      (define-values (u-visited u-distances)
        (for/fold ([u-visited (rest visited)]
                   [u-distances distances])
                  ([neighbor ns]
                   #:do ((define move-cost (hash-ref g (cons current neighbor) #f)))
                   #:when move-cost
                   #:do ((define through-current
                           (+ current-distance move-cost)))
                   #:when (< through-current (hash-ref distances neighbor +inf.0)))
                  (values (insert (remove neighbor visited)
                                  neighbor
                                  through-current
                                  distances)
                          (hash-set distances
                                    neighbor
                                    through-current))))
      (dijkstra g start u-visited u-distances)]))

(define (manhattan-distance from to)
  (+ (abs (- (car from) (car to)))
     (abs (- (cdr from) (cdr to)))))

(define (cheats corridors walls [l 2])
  (for*/list ([from corridors]
              [row (in-range (- 0 l) (add1 l))]
              [col (in-range (- 0 l) (add1 l))]
              #:do ((define to (cons (+ (car from) row)
                                     (+ (cdr from) col))))
              #:when (and (not (equal? to from))
                          (set-member? corridors to)
                          (<= (manhattan-distance from to) l)))
             (cons from to)))

(define (score-cheat cheat shortest-from-start shortest-from-end)
  (match-define (cons cheat-start cheat-end) cheat)
  (+ (hash-ref shortest-from-start cheat-start)
     (manhattan-distance cheat-start cheat-end)
     (hash-ref shortest-from-end cheat-end)))

(define (part-one)
  (define-values (start end corridors walls) (read-course))
  (define min-savings 100)
  (define course-graph (graph corridors walls))
  (define shortest-from-start (dijkstra course-graph start))
  (define shortest-from-end (dijkstra course-graph end))
  (define fastest-no-cheats (hash-ref shortest-from-start end))
  (define all-cheats (cheats corridors walls))
  (length
    (for/list ([cheat all-cheats]
               #:do ((define cheat-time
                       (score-cheat cheat
                                    shortest-from-start
                                    shortest-from-end))
                     (define savings (- fastest-no-cheats
                                        cheat-time)))
               #:when (>= savings min-savings))
              cheat)))

(with-input-from-file "input.txt" part-one)

(define (part-two)
  (define-values (start end corridors walls) (read-course))
  (define min-savings 100)
  (define course-graph (graph corridors walls))
  (define shortest-from-start (dijkstra course-graph start))
  (define shortest-from-end (dijkstra course-graph end))
  (define fastest-no-cheats (hash-ref shortest-from-start end))
  (define all-cheats (cheats corridors walls 20))
  (length
    (for/list ([cheat all-cheats]
               #:do ((define cheat-time
                       (score-cheat cheat
                                    shortest-from-start
                                    shortest-from-end))
                     (define savings (- fastest-no-cheats
                                        cheat-time)))
               #:when (>= savings min-savings))
              cheat)))

(with-input-from-file "input.txt" part-two)
