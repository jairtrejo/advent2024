#lang racket

(require qi)

(define (read-reports)
  (for/list ([l (in-lines)])
            (on (l)
              (~> string-split
                  (map string->number _)))))

(define (change-safe level-a level-b direction)
  (and (direction level-a level-b)
       (<= (abs (- level-a level-b)) 3)))

(define (report-safe report direction)
  (for/and ([a report]
            [b (drop report 1)])
           (change-safe a b direction)))

(define (remove-at at lst)
  (define-values (left right) (split-at lst at))
  (append left (cdr right)))

(define (report-safe-with-dampener report direction)
  (define failure-at
    (for/or ([a report]
             [b (drop report 1)]
             [at (in-naturals)])
            (if (change-safe a b direction) #f at)))
  (or (not failure-at)
      (report-safe (remove-at failure-at report) >)
      (report-safe (remove-at failure-at report) <)
      (report-safe (remove-at (add1 failure-at) report) <)
      (report-safe (remove-at (add1 failure-at) report) >)))

(define (part-one)
  (for/sum ([report (read-reports)])
           (define direction
             (if (< (first report) (second report)) < >))
           (if (report-safe report direction) 1 0)))

(define (part-two)
  (for/sum ([report (read-reports)]
            [i (in-naturals)])
           (if (or
                 (report-safe-with-dampener report <)
                 (report-safe-with-dampener report >))
               1 0)))

(with-input-from-file "input.txt"
  part-one)

(with-input-from-file "input.txt"
  part-two)
