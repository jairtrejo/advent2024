#lang racket

(define (read-onsen)
  (define towels
    (map string->list
         (string-split (read-line) ", ")))
  (read-line)
  (define designs
     (for/list ([l (in-lines)]) (string->list l)))
  (values towels designs))

(define cache (make-hash))

(define (possible towels design)
  (define result
    (if (hash-has-key? cache design)
        (hash-ref cache design)
        (cond
          [(empty? design) #t]
          [else (for/or ([t towels])
                        (define-values (t-left next)
                          (drop-common-prefix t design))
                        (cond
                          [(empty? t-left)
                           (possible towels next)]
                          [else #f]))])))
  (hash-set! cache design result)
  result)

(define (part-one)
  (hash-clear! cache)
  (define-values (towels designs) (read-onsen))
  (for/sum ([design designs]
            #:when (possible towels design))
           1))

(with-input-from-file "input.txt" part-one)

(define cache-count (make-hash))

(define (possible-count towels design)
  (define result
    (if (hash-has-key? cache-count design)
        (hash-ref cache-count design)
        (cond
          [(empty? design) 1]
          [else (for/sum ([t towels])
                         (define-values (t-left next)
                           (drop-common-prefix t design))
                         (cond
                           [(empty? t-left)
                            (possible-count towels next)]
                           [else 0]))])))
  (hash-set! cache-count design result)
  result)

(define (part-two)
  (hash-clear! cache-count)
  (define-values (towels designs) (read-onsen))
  (for/sum ([design designs])
           (possible-count towels design)))

(with-input-from-file "input.txt" part-two)
