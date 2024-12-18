#lang racket

(struct computer (a b c program ic stdout)
                 #:transparent)

(define (read-device)
  (define a (string->number (first (regexp-match #px"\\d+" (read-line)))))
  (define b (string->number (first (regexp-match #px"\\d+" (read-line)))))
  (define c (string->number (first (regexp-match #px"\\d+" (read-line)))))
  (read-line)
  (define program
    (list->vector
      (map string->number
           (string-split (second (string-split (read-line) ": "))
                         ","))))
  (computer a b c program 0 ""))
(define the-device
  (with-input-from-file "example.txt" read-device))
the-device

(define literal identity)

(define (combo computer operand)
  (match operand
    [(or 0 1 2 3) (literal operand)]
    [4 (computer-a computer)]
    [5 (computer-b computer)]
    [6 (computer-c computer)]))

(define (adv device operand)
  (define num (computer-a device))
  (define den (arithmetic-shift 1 (combo device operand)))
  (define res (truncate (/ num den)))
  (struct-copy computer device
               [a res]))
(adv (computer 9 2 0 #() 0 "") 1)

(define (bxl device operand)
  (define a (computer-b device))
  (define b (literal operand))
  (define res (bitwise-xor a b))
  (struct-copy computer device
               [b res]))
(bxl (computer 0 4 0 #() 0 "") 5)

(define (bst device operand)
  (define res (modulo (combo device operand) 8))
  (struct-copy computer device
               [b res]))
(bst (computer 11 0 0 #() 0 "") 4)

(define (jnz device operand)
  (define to (literal operand))
  (if (zero? (computer-a device))
      device
      (struct-copy computer device
                   [ic (- to 2)])))
(jnz (computer 3 0 0 #() 3 "") 1)

(define (bxc device)
  (define a (computer-b device))
  (define b (computer-c device))
  (define res (bitwise-xor a b))
  (struct-copy computer device
               [b res]))
(bxc (computer 0 4 5 #() 0 ""))

(define (out device operand)
  (define v (modulo (combo device operand) 8))
  (define current (computer-stdout device))
  (define res (string-trim (string-append current
                                          (~a "," v))
                           ","))
  (struct-copy computer device
               [stdout res]))
(out (computer 0 4 5 #() 0 "7") 3)

(define (bdv device operand)
  (define num (computer-a device))
  (define den (arithmetic-shift 1 (combo device operand)))
  (define res (truncate (/ num den)))
  (struct-copy computer device
               [b res]))
(bdv (computer 9 2 0 #() 0 "") 1)

(define (cdv device operand)
  (define num (computer-a device))
  (define den (arithmetic-shift 1 (combo device operand)))
  (define res (truncate (/ num den)))
  (struct-copy computer device
               [c res]))
(cdv (computer 9 2 0 #() 0 "") 1)

(define (execute-one device)
  (match-define (computer a b c program ic stdout) device)
  (cond
    [(>= ic (sub1 (vector-length program))) #f]
    [else
      (define opcode (vector-ref program ic))
      (define operand (vector-ref program (add1 ic)))
      (define step
        (match opcode
          [0 (adv device operand)]
          [1 (bxl device operand)]
          [2 (bst device operand)]
          [3 (jnz device operand)]
          [4 (bxc device)]
          [5 (out device operand)]
          [6 (bdv device operand)]
          [7 (cdv device operand)]))
      (struct-copy computer step
                   [ic (+ (computer-ic step) 2)])]))

(define (run device)
  (define next (execute-one device))
  (if next
      (run next)
      device))
(run (computer 0 0 9 #(2 6) 0 ""))
(run (computer 10 0 0 #(5 0 5 1 5 4) 0 ""))
(run (computer 2024 0 0 #(0 1 5 4 3 0) 0 ""))
(run (computer 0 29 0 #(1 7) 0 ""))
(run (computer 0 2024 43690 #(4 0) 0 ""))
(run the-device)

(define (part-one)
  (define device (read-device))
  (define final (run device))
  (computer-stdout final))
(with-input-from-file "example.txt" part-one)

(with-input-from-file "input.txt" part-one)

(define (part-two)
  (define device (read-device))
  (define program (computer-program device))
  (define program-text (apply ~a
                              (vector->list program)
                              #:seprator ","))
  (displayln program-text))
  ;; (let loop ([d device]
  ;;            [o (computer-stdout d)])
  ;;   (define next (execute-one d))
  ;;   (cond
  ;;     [(not next) (if (equal? o))])))
(with-input-from-file "example.txt" part-two)
