#lang racket

(define (read-machine)
  (map string->number
    (append*
      (for/list ([i 4]
                 [l (in-lines)])
                (regexp-match* #px"\\d+" l)))))

(define (read-machines)
  (define machine (read-machine))
  (cond
    [(empty? machine) '()]
    [else (cons machine (read-machines))]))

(define (button-presses machine)
  (match-define (list xa ya xb yb xp yp)
                machine)
  (define-values (a b)
    (cond
      [(= (* xa yb) (* xb ya))
       (define px (/ xp (min xa xb)))
       (define py (/ yp (min ya yb)))
       (if (= px py)
        (for*/fold ([cheapest +inf.0]
                    [cheapest-a #f]
                    [cheapest-b #f]
                    #:result (values cheapest-a
                                     cheapest-b))
                   ([a 101]
                    [b 101]
                    #:when (= xp (+ (* a xa)
                                    (* b xb))))
                  (define cost (+ b (* 3 a)))
                  (if (< cost cheapest)
                      (values cost a b)
                      (values cheapest cheapest-a cheapest-b)))
        (values #f #f))]
      [else
        (define b
          (/ (- (* xa yp) (* ya xp))
             (- (* xa yb) (* xb ya))))
        (define a
          (/ (- xp (* xb b)) xa))
        (values a b)]))
  (if (and (integer? a) (integer? b)
           (>= a 0) (>= b 0))
    (values a b)
    (values #f #f)))

(define (part-one)
  (define machines (read-machines))
  (for/sum ([machine machines]
            #:do ((define-values (a b)
                                 (button-presses machine)))
            #:when (and a b))
           (+ (* 3 a) b)))

(with-input-from-file "input.txt" part-one)

(define (part-two)
  (define machines (read-machines))
  (for/sum ([machine machines]
            #:do ((match-define (list xa ya xb yb xp yp) machine)
                  (define new-machine (list xa ya xb yb (+ 10000000000000 xp) (+ 10000000000000 yp)))
                  (define-values (a b)
                                 (button-presses new-machine)))
            #:when (and a b))
           (+ (* 3 a) b)))

(with-input-from-file "input.txt" part-two)
