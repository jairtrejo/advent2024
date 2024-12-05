#lang racket

(define (read-field)
  (for/list ([l (in-lines)])
            (string->list l)))

(define (field-size f)
  (values (length (first f))
          (length f)))

(define (field-at f x y)
  (define-values (w h) (field-size f))
  (list-ref (list-ref f (sub1 (- h y))) x))

(define (get-column f x)
  (define-values (w h) (field-size f))
  (list->string
    (for/list ([y h])
              (field-at f x y))))

(define (get-columns f)
  (define-values (w h) (field-size f))
  (for/list ([x w])
            (get-column f x)))

(define (get-row f y)
  (define-values (w h) (field-size f))
  (list->string
    (for/list ([x w])
              (field-at f x y))))

(define (get-rows f)
  (define-values (w h) (field-size f))
  (for/list ([y h])
            (get-row f y)))

(define (in-field? f x y)
  (define-values (w h) (field-size f))
  (and (< -1 x w) (< -1 y h)))

(define (get-diagonal f m b)
  (define-values (w h) (field-size f))
  (list->string
    (for/list ([x w]
               #:do [(define y (+ b (* m x)))]
               #:when (in-field? f x y))
      (field-at f x y))))

(define (get-forward-diagonals f)
  (define-values (w h) (field-size f))
  (for/list ([b (in-range (- h) h)])
            (get-diagonal f 1 b)))

(define (get-backward-diagonals f)
  (define-values (w h) (field-size f))
  (for/list ([b (in-range 0 (* 2 h))])
            (get-diagonal f -1 b)))

(define (get-diagonals f)
  (define-values (w h) (field-size f))
  (append
    (get-forward-diagonals f)
    (get-backward-diagonals f)))

(define (xmas-count s)
  (+ (length (regexp-match* "XMAS" s))
     (length (regexp-match* "SAMX" s))))

(define (part-one)
  (define a-field (read-field))
  (define lines
    (append (get-columns a-field)
            (get-rows a-field)
            (get-diagonals a-field)))
  (for/sum ([l lines])
           (xmas-count l)))

(define (get-diagonal-positions f m b)
  (define-values (w h) (field-size f))
  (for/list ([x w]
             #:do [(define y (+ b (* m x)))]
             #:when (in-field? f x y))
    (cons x y)))

(define (mas-middles s)
  (define mas-positions
    (append (regexp-match-positions* "MAS" s)
            (regexp-match-positions* "SAM" s)))
  (map (lambda (p) (add1 (car p))) mas-positions))

(define (mas-centers f m b)
  (define s (get-diagonal f m b))
  (define ps (get-diagonal-positions f m b))
  (define cs (mas-middles s))
  (map (curry list-ref ps) cs))

(define (forward-centers f)
  (define-values (w h) (field-size f))
  (for*/list ([b (in-range (- h) h)]
              #:do [(define centers (mas-centers f 1 b))]
              #:when (not (empty? centers))
              [center centers])
             center))

(define (backward-centers f)
  (define-values (w h) (field-size f))
  (for*/list ([b (in-range 0 (* 2 h))]
              #:do [(define centers (mas-centers f -1 b))]
              #:when (not (empty? centers))
              [center centers])
             center))

(define (part-two)
  (define a-field (read-field))
  (define fwd (list->set (forward-centers a-field)))
  (define bwd (list->set (backward-centers a-field)))
  (set-count (set-intersect fwd bwd)))

(with-input-from-file "input.txt"
  part-one)

(with-input-from-file "input.txt"
  part-two)
