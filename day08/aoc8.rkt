#lang racket

(define (read-antennas)
  (for/fold ([antennas (hash)]
             [size #f])
            ([row (in-naturals)]
             [line (in-lines)]
             #:when #t
             [col (in-naturals)]
             [c line])
            (define size (cons (add1 row) (add1 col)))
            (match c
              [(not #\.) (values (hash-update antennas
                                              c
                                              (curry cons (make-rectangular row col))
                                              (list))
                                size)]
              [else (values antennas size)])))

(define (antinodes antenna-a antenna-b)
  (define difference (- antenna-b antenna-a))
  (define distance (magnitude difference))
  (define unit-difference (/ difference distance))
  (map (λ (c) (make-rectangular (exact-floor (real-part c))
                                (exact-floor (imag-part c))))
       (list (+ antenna-a (* unit-difference distance 2))
             (- antenna-b (* unit-difference distance 2)))))

(define (within? point size)
  (and (< -1 (real-part point) (car size))
       (< -1 (imag-part point) (cdr size))))
(within? (make-rectangular 0 11) the-size)

(define (part-one)
  (define-values (antennas size) (read-antennas))
  (set-count
    (for*/set ([batch (hash-values antennas)]
               [antenna-pair (combinations batch 2)]
               [antinode (apply antinodes antenna-pair)]
               #:when (within? antinode size))
              antinode)))

(define (antinodes-harmonics steps antenna-a antenna-b)
  (define difference (- antenna-b antenna-a))
  (define simplified (/ difference
                        (gcd (real-part difference)
                             (imag-part difference))))
  (append*
    (for/list ([s steps])
              (list
                (+ antenna-a (* s simplified))
                (- antenna-a (* s simplified))))))

(define (part-two)
  (define-values (antennas size) (read-antennas))
  (define steps (exact-ceiling (sqrt (+ (expt (car size) 2)
                                        (expt (cdr size) 2)))))
  (set-count
    (for*/set ([batch (hash-values antennas)]
               [antenna-pair (combinations batch 2)]
               [antinode (apply antinodes-harmonics steps antenna-pair)]
               #:when (within? antinode size))
              antinode)))

(with-input-from-file "input.txt" part-one)

(with-input-from-file "input.txt" part-two)
