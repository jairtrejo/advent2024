#lang racket

(define (read-rules)
  (for/list ([l (in-lines)]
             #:break (equal? l ""))
            (map string->number (string-split l "|"))))

(define (read-updates)
  (for/list ([l (in-lines)])
            (map string->number (string-split l ","))))

(define-values (the-rules the-updates)
  (with-input-from-file "example.txt"
    (λ () (values (read-rules) (read-updates)))))

(define (less-than rules a b)
  (for/or ([rule rules])
       (define before (first rule))
       (define after (second rule))
       (and (eq? a before) (eq? b after))))

(define (already-sorted? rules update)
  (define sorted (sort update (curry less-than rules)))
  (equal? sorted update))

(define (middle-page update)
  (list-ref update (floor (/ (length update) 2))))

(define (part-one)
  (define the-rules (read-rules))
  (define the-updates (read-updates))
  (for/sum ([update the-updates]
            #:when (already-sorted? the-rules update))
           (middle-page update)))

(define (part-two)
  (define the-rules (read-rules))
  (define the-updates (read-updates))
  (for/sum ([update the-updates]
            #:unless (already-sorted? the-rules update))
           (middle-page (sort update (curry less-than the-rules)))))

(with-input-from-file "input.txt"
  part-one)

(with-input-from-file "input.txt"
  part-two)
