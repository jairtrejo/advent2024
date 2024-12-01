#lang racket

(require qi)

(define (read-lists)
  (for/lists (left right
              #:result (values (sort left <) (sort right <)))
             ([l (in-lines)]
              #:when (non-empty-string? l))
             (on (l)
               (~> (string-split _ #:repeat? #t)
                   (map string->number _)
                   (-< first second)))))

(define (part-one)
  (define-values (left right) (read-lists))
  (for/sum ([l left]
            [r right])
           (abs (- l r))))

(define (part-two)
  (define-values (left right) (read-lists))
  (for/sum ([l left])
           (define appearances (count (λ (r) (eq? r l)) right))
           (* l appearances)))

(with-input-from-file "example.txt"
  part-one)

(with-input-from-file "example.txt"
  part-two)
