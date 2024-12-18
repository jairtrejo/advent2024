#lang racket

(define (read-falling-bytes)
  (for/fold ([falling-bytes (list)]
             #:result (reverse falling-bytes))
            ([line (in-lines)])
            (define coords (map string->number
                                (string-split line ",")))
            (define falling-byte (cons (first coords)
                                       (second coords)))
            (cons falling-byte falling-bytes)))

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

(define (graph width height fallen-bytes)
  (define obstacles (list->set fallen-bytes))
  (define spaces (for*/list ([x width]
                             [y height]
                             #:do ((define space (cons x y)))
                             #:unless (set-member? obstacles
                                                   space))
                            space))
  (for*/hash ([from spaces]
              [dir directions]
              #:do ((define to (dir from)))
              #:unless (set-member? obstacles to))
             (values (cons from to) 1)))

(define (neighbors graph node)
  (for/set ([d directions]
            #:do ((define to (d node)))
            #:when (hash-has-key? graph (cons node to)))
           to))

(define (h-distance from to)
  (+ (abs (- (car from) (car to)))
     (abs (- (cdr from) (cdr to)))))

(define (insert queue node priority priorities)
  (define-values (before after)
    (splitf-at queue (λ (e) (> priority (hash-ref priorities e)))))
  (append before (cons node after)))

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
        [(equal? current end) (hash-ref gscore current)]
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

(define (part-one)
  (define-values (w h) (values 71 71))
  (define exit (cons (sub1 w) (sub1 h)))
  (define falling-bytes (read-falling-bytes))
  (define fallen-bytes (take falling-bytes 1025))
  (define safe-graph (graph w h fallen-bytes))
  (a-star safe-graph (cons 0 0) exit))

(with-input-from-file "input.txt" part-one)

(define (binary-search mi ma goal)
  (define mid (ceiling (+ mi (/ (- ma mi) 2))))
  (define midv (goal mid))
  (cond
    [(< (- ma mi) 2) ma]
    [(not midv) (binary-search mi mid goal)]
    [midv (binary-search mid ma goal)]))

(define (part-two)
  (define-values (w h) (values 71 71))
  (define start (cons 0 0))
  (define exit (cons (sub1 w) (sub1 h)))
  (define falling-bytes (read-falling-bytes))
  (define (goal n)
    (define fallen-bytes (take falling-bytes (add1 n)))
    (define safe-graph (graph w h fallen-bytes))
    (a-star safe-graph (cons 0 0) exit))
  (define breaks-at
    (binary-search 0 (length falling-bytes) goal))
  (define breaks-byte
    (list-ref falling-bytes breaks-at))
  (~a (car breaks-byte) (cdr breaks-byte) #:separator ","))

(with-input-from-file "input.txt" part-two)
