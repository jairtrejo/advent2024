#lang racket

(define (read-connections)
  (for/list ([l (in-lines)])
    (string-split l "-")))

(define (all-connected connections)
  (for*/fold ([all (hash)])
             ([connection connections]
              [computers (list connection (reverse connection))])
             (hash-update all
                          (first computers)
                          (curryr set-add (second computers))
                          (set))))

(define (part-one)
  (define connections (read-connections))
  (define connected (all-connected connections))
  (set-count
    (for*/set ([com1 (hash-keys connected)]
               #:when (string-prefix? com1 "t")
               #:do ((define connected-to-com1
                        (set->list (hash-ref connected com1))))
               [pair (combinations connected-to-com1 2)]
               #:do ((match-define (list com2 com3) pair)
                     (define connected-to-com2
                       (hash-ref connected com2)))
               #:when (set-member? connected-to-com2 com3))
              (set com1 com2 com3))))

(with-input-from-file "input.txt" part-one)

(define (fully-connected connected)
  (for/fold ([groups (list)]
             #:result (sort groups > #:key set-count))
            ([com (hash-keys connected)])
            (define connected-to-com
              (hash-ref connected com))
            (cons (set com)
                  (map (λ (group)
                         (if (subset? group connected-to-com)
                             (set-add group com)
                             group))
                       groups))))

(define (part-two)
  (define connections (read-connections))
  (define connected (all-connected connections))
  (define party (first (fully-connected connected)))
  (string-join (sort (set->list party) string<?) ","))

(with-input-from-file "input.txt" part-two)
