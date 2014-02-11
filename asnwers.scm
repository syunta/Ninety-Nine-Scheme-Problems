; No.1
(define (single? seq)
  (and (pair? seq) (null? (cdr seq))))

; No.2
(define (double? seq)
  (and (pair? seq) (single? (cdr seq))))

; No.3
(define (longer?-1 seq-x seq-y)
  (> (length seq-x) (length seq-y)))

(define (longer?-2 seq-x seq-y)
  (if (pair? seq-x) 
    (if (pair? seq-y)
      (longer? (cdr seq-x) (cdr seq-y))
      #t)
    #f))

; No.4
(define (last seq)
  (if (null? (cdr seq))
    seq
    (last (cdr seq))))

(define (butlast seq)
  (if (null? (cdr seq))
    nil
    (cons (car seq) (butlast (cdr seq)))))

; No.5
(define (take seq n)
  (if (and (not (= n 0)) (pair? seq))
    (cons (car seq) (take (cdr seq) (- n 1)))
    nil))

; No.6
(define (drop seq n)
  (if (or (= n 0) (null? seq))
    seq
    (drop (cdr seq) (- n 1))))
