; No.1
(define (single? seq)
  (and (pair? seq) (null? (cdr seq))))
