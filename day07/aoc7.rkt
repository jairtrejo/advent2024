#lang racket

(define (read-calibrations)
  (for/list ([l (in-lines)])
    (map string->number (string-split l #rx"[: ]+"))))

(define (to-base n base)
  (cond
    [(< n base) (list n)]
    [else (cons (modulo n base) (to-base (/ (- n (modulo n base)) base) base))]))

(define (decode number all-operators)
  (map (curry list-ref all-operators) (to-base number (length all-operators))))

(define (operate operators operands)
  (for/fold ([total (first operands)])
            ([operand (rest operands)]
             [operator operators])
            (operator total operand)))

(define (pad lst len e)
  (cond [(>= (length lst) len) lst]
        [else (pad (cons e lst) len e)]))

(define (can-solve? calibration all-operators)
  (define max-encoded (sub1 (expt (length all-operators) (- (length calibration) 2))))
  (for/or ([encoded (add1 max-encoded)]
           #:do ((define needed-operators (- (length calibration) 2))
                 ;; (displayln (decode encoded all-operators))
                 (define operators (reverse (pad (reverse (decode encoded all-operators))
                                                 needed-operators
                                                 (first all-operators))))
                 (define total (operate operators
                                        (rest calibration)))))
          (if (and
               (= total (first calibration))
               (> encoded max-encoded))
              (displayln (~a calibration " (" max-encoded "): " encoded " " operators)) #f)
          (= total (first calibration))))

(define (part-one)
  (define calibrations (read-calibrations))
  (for/sum ([calibration calibrations]
            #:when (can-solve? calibration (list + *)))
           (first calibration)))

(with-input-from-file "input.txt" part-one)

(define (concat a b)
  (string->number (string-append (number->string a) (number->string b))))

(define (part-two)
  (define calibrations (read-calibrations))
  (for/sum ([calibration calibrations]
            #:when (can-solve? calibration (list + concat *)))
           (first calibration)))

(with-input-from-file "input.txt" part-two)
