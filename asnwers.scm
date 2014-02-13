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

; No.7
(define (subseq seq n m)
  (take (drop seq n) (- m n)))

; No.8
(define (butlast-n-1 seq n)
  (if (or (= n 0) (null? seq))
    seq
    (butlast-n-1 (butlast seq) (- n 1))))

(define (butlast-n-2 seq n)
  (let ((len (length seq)))
    (let ((m (if (> n len)
              len
              n)))
      (take seq (- len m)))))

; No.9
(define (group seq n)
  (if (null? seq)
    seq
    (cons (take seq n) (group (drop seq n) n))))

; No.10
(define (position x seq)
  (define (count-pos x seq n)
    (cond
      ((null? seq) #f)
      ((equal? x (car seq)) n)
      (else (count-pos x (cdr seq) (+ n 1)))))
  (count-pos x seq 0))

; No.11
(define (count x seq)
  (define (count-x x seq n)
    (cond
      ((null? seq) n)
      ((equal? x (car seq)) (count-x x (cdr seq) (+ n 1)))
      (else (count-x x (cdr seq) n))))
  (count-x x seq 0))

; No.12
(define (sum-list seq)
  (define (current-sum seq sum)
    (if (null? seq)
      sum
      (current-sum (cdr seq) (+ sum (car seq)))))
  (current-sum seq 0))

; No.13
(define (max-list seq)
  (define (max-element seq mx)
    (cond
      ((null? seq) mx)
      ((> (car seq) mx)
        (max-element (cdr seq) (car seq)))
      (else (max-element (cdr seq) mx))))
  (if (null? seq)
    seq
    (max-element seq (car seq))))

(define (min-list seq)
  (define (min-element seq mn)
    (cond
      ((null? seq) mn)
      ((< (car seq) mn)
        (min-element (cdr seq) (car seq)))
      (else (min-element (cdr seq) mn))))
  (if (null? seq)
    seq
    (min-element seq (car seq))))

; No.14
(define (adjacent? x y seq)
  (cond
    ((or (null? seq) (null? (cdr seq))) #f)
    ((and (equal? x (car seq)) (equal? y (cadr seq))) #t)
    (else (adjacent? x y (cdr seq)))))

; No.15
(define (before?-1 x y seq)
  (define (search x-or-y seq y?)
    (cond
      ((null? seq) #f)
      ((equal? x-or-y (car seq)) 
       (if y?
         seq
         (search y (cdr seq) #t)))
      (else (search x-or-y (cdr seq) y?))))
  (search x seq #f))

(define (before?-2 x y seq)
  ((lambda (after-x)
     (if after-x
       (member y (cdr after-x))
       #f))
   (member x seq)))

; No.16
(define (iota n m)
  (if (> n m)
    nil
    (cons n (iota (+ n 1) m))))
