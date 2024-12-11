#lang racket

(define (read-stones)
  (map string->number (string-split (read-line))))

(define (split s)
  (define stone (number->string s))
  (define len (string-length stone))
  (map string->number
       (list (substring stone 0 (/ len 2))
             (substring stone (/ len 2)))))

(define (blink stones)
  (flatten
    (for/list ([stone stones])
              (cond [(= stone 0) 1]
                    [(even? (string-length (number->string stone)))
                     (split stone)]
                    [else (* 2024 stone)]))))

(define (part-one)
  (define stones (read-stones))
  (for/fold ([stones stones]
             #:result (length stones))
            ([_ 25])
            (blink stones)))

(with-input-from-file "input.txt" part-one)

(define cache (make-hash))

(define (blink-length stone n)
  (define cache-key (cons stone n))
  (if (hash-has-key? cache cache-key)
      #f
      (hash-set! cache
                 cache-key
                 (cond [(= n 0) 1]
                       [(= stone 0) (blink-length 1 (sub1 n))]
                       [(even? (string-length (number->string stone)))
                        (for/sum ([s (split stone)]) (blink-length s (sub1 n)))]
                       [else (blink-length (* 2024 stone) (sub1 n))])))
  (hash-ref cache cache-key))

(define (part-two)
  (define stones (read-stones))
  (for/sum ([stone stones])
           (blink-length stone 75)))

(with-input-from-file "input.txt" part-two)
