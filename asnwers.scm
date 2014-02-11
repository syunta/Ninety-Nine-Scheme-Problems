; No.1
(define (single? seq)
  (and (pair? seq) (null? (cdr seq))))

; No.2
(define (double? seq)
  (and (pair? seq) (single? (cdr seq))))
