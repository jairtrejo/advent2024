#lang racket

(define (read-calibrations)
  (for/list ([l (in-lines)])
    (map string->number (string-split l #rx"[: ]+"))))
(with-input-from-file "example.txt"
  read-calibrations)

(define (decode number [mask 1])
  (cond
    [(> mask number) (if (positive? number) '() (list +))]
    [else (cons (if (zero? (bitwise-and mask number)) + *)
                (decode number (arithmetic-shift mask 1)))])) 

(define (operate operators operands)
  (for/fold ([total (first operands)])
            ([operand (rest operands)]
             [operator operators])
            (operator total operand)))
(operate (list +) '(1 3))

(define (can-solve? calibration)
  (define max-encoded-operators (sub1 (expt 2 (length calibration))))
  (for/or ([encoded-operators max-encoded-operators]
           #:do ((define total (operate (decode encoded-operators) (rest calibration)))))
          (= total (first calibration))))
(can-solve? '(190 10 19))

(define (part-one)
  (define calibrations (read-calibrations))
  (for/sum ([calibration calibrations]
            #:when (can-solve? calibration))
           (first calibration)))

;; (define (to-base n base)
;;   (cond
;;     [(< n base) (list n)]
;;     [else (cons (modulo n base) (to-base (/ (- n (modulo n base)) base) base))]))
;; (to-base 0 3)

(with-input-from-file "input.txt" part-one)
