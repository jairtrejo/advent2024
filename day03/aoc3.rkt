#lang racket

(define (get-mul s)
  (define matches (regexp-match #px"^mul\\([0-9]{1,3},[0-9]{1,3}\\)" s))
  (if matches (first matches) #f))

(define (get-instruction s)
  (cond
    [(get-mul s)]
    [(string-prefix? s "do()") "do()"]
    [(string-prefix? s "don't()") "don't()"]
    [else #f]))

(define (collect-instructions s)
  (define instruction (get-instruction s))
  (cond [(equal? s "") '()]
        [instruction
         (cons instruction (collect-instructions (substring s 1)))]
        [else (collect-instructions (substring s 1))]))

(define (get-operands instruction)
  (define matches (regexp-match* #rx"[0-9]+" instruction))
  (apply values (map string->number matches)))

(define (execute-mul instruction)
  (define-values (a b) (get-operands instruction))
  (* a b))

(define (part-one)
  (for*/sum ([l (in-lines)]
             [instruction (collect-instructions l)])
            (if (string-prefix? instruction "mul")
                (execute-mul instruction)
                0)))

(define (part-two)
  (for*/fold ([total 0] [enabled #t] #:result total)
             ([l (in-lines)]
              [instruction (collect-instructions l)])
             (cond
               [(and enabled (string-prefix? instruction "mul"))
                (values (+ total (execute-mul instruction)) enabled)]
               [(and (not enabled) (string-prefix? instruction "mul"))
                (values total enabled)]
               [(string=? instruction "do()") (values total #t)]
               [(string=? instruction "don't()") (values total #f)])))

(with-input-from-file "input.txt"
  part-one)

(with-input-from-file "input.txt"
  part-two)
