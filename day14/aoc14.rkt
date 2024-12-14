#lang racket

(define (read-robots)
  (for/list ([l (in-lines)])
    (match-define (list px py vx vy)
                  (map string->number
                    (regexp-match* #px"-?\\d+" l)))
    (cons (make-rectangular px py)
          (make-rectangular vx vy))))

(define (wrap bounds pos)
  (make-rectangular
    (modulo (real-part pos) (real-part bounds))
    (modulo (imag-part pos) (imag-part bounds))))

(define (move-robot bounds time robot)
  (match-define (cons p v) robot)
  (cons (wrap bounds (+ p (* time v))) v))

(define (quadrant bounds robot)
  (define x-axis (floor (/ (real-part bounds) 2)))
  (define y-axis (floor (/ (imag-part bounds) 2)))
  (match-define (cons p _) robot)
  (define x (real-part p))
  (define y (imag-part p))
  (cond 
    [(and (< x x-axis) (< y y-axis)) 1]
    [(and (> x x-axis) (< y y-axis)) 2]
    [(and (< x x-axis) (> y y-axis)) 3]
    [(and (> x x-axis) (> y y-axis)) 4]
    [else #f]))

(define (count-by-quadrant bounds robots)
  (for/fold ([counts (hash)])
            [(robot robots)]
            (hash-update counts
                         (quadrant bounds robot)
                         add1
                         0))) 

(define (part-one)
  (define robots (read-robots))
  (define bounds (make-rectangular 101 103))
  (define moved-robots (map (curry move-robot bounds 100)
                            robots))
  (define by-quadrant (count-by-quadrant bounds moved-robots))
  (for/product ([(q c) by-quadrant]
                #:when q)
               c))

(with-input-from-file "input.txt" part-one)

(define (draw-robots bounds robots)
  (define width (real-part bounds))
  (define height (imag-part bounds))
  (define robot-positions (apply set (map car robots)))
  (displayln "P1")
  (display (~a (real-part bounds) " " (imag-part bounds)))
  (for* ([y height]
         #:do((displayln ""))
         [x width])
        (define pos (make-rectangular x y))
        (display (if (set-member? robot-positions pos)
                     "0 " "1 "))))

(define (messiness robots)
  (define pos (map car robots))
  (define xs (map real-part pos))
  (define ys (map imag-part pos))
  (define x-mean (/ (apply + xs) (length xs)))
  (define y-mean (/ (apply + ys) (length ys)))
  (define x-var (apply + (map (λ (x) (expt (- x x-mean) 2)) xs)))
  (define y-var (apply + (map (λ (y) (expt (- y y-mean) 2)) ys)))
  (* x-var y-var))

(define (part-two)
  (define robots (read-robots))
  (define bounds (make-rectangular 101 103))
  (define tidiest
    (for/fold ([min-messiness +inf.0]
               [tidiest-t #f]
               #:result tidiest-t)
              ([t 10000])
              (define moved-robots
                (map (curry move-robot bounds t)
                     robots))
              (define mess (messiness moved-robots))
              (if (< mess min-messiness)
                  (values mess t)
                  (values min-messiness tidiest-t))))
  (define tidy-robots (map (curry move-robot bounds tidiest)
                           robots))
  (with-output-to-file "tidy.pbm" #:exists 'replace
    (λ () (draw-robots bounds tidy-robots)))
  tidiest)

(with-input-from-file "input.txt" part-two)
