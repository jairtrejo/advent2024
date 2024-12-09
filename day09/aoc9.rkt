#lang racket

(define (read-disk)
  (filter-map string->number (string-split (read-line) "")))

(define (offsets disk)
  (for/fold ([offset 0]
             [all-offsets '()]
             #:result (reverse all-offsets))
            ([n disk])
            (values (+ n offset)
                    (cons offset all-offsets))))

(struct section (id offset len) #:transparent)

(define (with-ids sections)
  (define len (length sections))
  (for/list ([s sections]
             [i (in-naturals)])
            (struct-copy section s [id (sub1 (- len i))])))

(define (split-sections disk)
  (for/fold ([files '()]
             [spaces '()]
             #:result (values (with-ids files)
                              (reverse spaces)))
            ([n disk]
             [offset (offsets disk)]
             [is-space (in-cycle '(#f #t))])
            (if is-space
                (values files
                        (cons (section #f offset n) spaces))
                (values (cons (section #f offset n) files)
                        spaces))))

(define (move file-section space-section)
  (cond [(or (= (section-len file-section) 0)
             (= (section-len space-section) 0))
         (values #f #f)]
        [(= (section-len file-section) (section-len space-section))
         (values (struct-copy section file-section [offset (section-offset space-section)])
                 #f)]
        [(< (section-len file-section) (section-len space-section))
         (values (struct-copy section
                              file-section
                              [offset (section-offset space-section)]
                              [len (section-len file-section)])
                 (struct-copy section
                              space-section
                              [offset (+ (section-offset space-section)
                                         (section-len file-section))]
                              [len (- (section-len space-section)
                                      (section-len file-section))]))]
        [(> (section-len file-section) (section-len space-section))
         (values (struct-copy section
                              file-section
                              [offset (section-offset space-section)]
                              [len (section-len space-section)])
                 (struct-copy section
                              file-section
                              [len (- (section-len file-section)
                                      (section-len space-section))]))]))

(define/match (compact files spaces [moved-files '()])
  [('() _ _) (append files moved-files)]
  [(_ '() _) (append files moved-files)]
  [((cons sf files-rest) (cons ss spaces-rest) _)
   (cond [(> (section-offset ss) (section-offset sf))
          (append files moved-files)]
         [else
          (define-values (moved leftover) (move sf ss))
          (match* (moved leftover)
            [(#f #f) (compact files spaces-rest moved-files)]
            [(_ #f) (compact files-rest spaces-rest (cons moved moved-files))]
            [(_ (section #f o l)) (compact files-rest (cons leftover spaces-rest) (cons moved moved-files))]
            [(_ (section i o l)) (compact (cons leftover files-rest) spaces-rest (cons moved moved-files))])])])

(define (checksum s)
  (for/sum ([i (section-len s)])
           (* (section-id s) (+ i (section-offset s)))))

(define (part-one)
  (define disk-map (read-disk))
  (define-values (files spaces) (split-sections disk-map))
  (for/sum ([s (compact files spaces)])
           (checksum s)))

(with-input-from-file "input.txt" part-one)

(define (index-fitting spaces fs)
  (index-where spaces
               (λ (ss) (>= (section-len ss)
                           (section-len fs)))))

(define (defrag files spaces)
  (for/fold ([moved-files '()]
             [unmoved-files '()]
             [spaces spaces]
             #:result (append moved-files unmoved-files))
            ([fs files])
            (define ssi (index-fitting spaces fs))
            (define ss (and ssi (list-ref spaces ssi)))
            (if (and ss (< (section-offset ss)
                           (section-offset fs)))
                (begin
                  (let-values
                    ([(moved leftover) (move fs ss)])
                    (values (cons moved moved-files)
                            unmoved-files
                            (if leftover (list-set spaces ssi leftover)
                                         (list-set spaces ssi (section #f 0 0))))))
                (values moved-files
                        (cons fs unmoved-files)
                        spaces))))

(define (visualize sections)
  (define sorted (sort sections < #:key section-offset))
  (string-join
    (for*/list ([s sorted]
                [i (section-len s)])
               (if (section-id s)
                 (number->string (section-id s))
                 "."))))

(define (part-two)
  (define disk-map (read-disk))
  (define-values (files spaces) (split-sections disk-map))
  (for/sum ([s (defrag files spaces)])
           (checksum s)))

(with-input-from-file "input.txt" part-two)
