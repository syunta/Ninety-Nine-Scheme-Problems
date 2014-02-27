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
  (if (or (= n 0) (not (pair? seq)))
    nil
    (cons (car seq) (take (cdr seq) (- n 1)))))

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

; No.17
(define (set-of-list seq)
  (cond
    ((null? seq) seq)
    ((member (car seq) (cdr seq)) (set-of-list (cdr seq)))
    (else (cons (car seq) (set-of-list (cdr seq))))))

; No.18
(define (union-1 x y)
  (set-of-list (append x y)))

(define (union-2 x y)
  (cond
    ((null? x) y)
    ((member (car x) y) (union (cdr x) y))
    (else (cons (car x) (union (cdr x) y)))))

; No.19
(define (intersection x y)
  (cond
    ((null? x) nil)
    ((member (car x) y) (cons (car x) (intersection (cdr x) y)))
    (else (intersection (cdr x) y))))

; No.20
(define (difference x y)
  (cond
    ((null? x) nil)
    ((member (car x) y) (difference (cdr x) y))
    (else (cons (car x) (difference (cdr x) y)))))

; No.21
(define (merge-list-1 op xs ys)
  (define (merge-x op xs ys)
    (cond
      ((null? ys) xs)
      ((op (car ys) (car xs))
       (cons (car ys) (merge-x op xs (cdr ys))))
      (else (cons (car xs) (merge-y op (cdr xs) ys)))))
  (define (merge-y op xs ys)
    (cond
      ((null? xs) ys)
      ((op (car xs) (car ys))
       (cons (car xs) (merge-y op (cdr xs) ys)))
      (else (cons (car ys) (merge-x op xs (cdr ys))))))
  (merge-y op xs ys))

(define (merge-list-2 op xs ys)
    (cond
      ((null? xs) ys)
      ((null? ys) xs)
      ((op (car xs) (car ys))
       (cons (car xs) (merge-list-2 op (cdr xs) ys)))
      (else (cons (car ys) (merge-list-2 op xs (cdr ys))))))

; No.22
(define (merge-sort op seq)
  (define (merge-iter op seq)
    (cond
      ((null? seq) nil)
      ((null? (cdr seq)) seq)
      (else (merge-iter op (cons
              (merge-list op (car seq) (cadr seq))
              (merge-iter op (cddr seq)))))))
  (define (split seq)
    (group seq 1))
  (car (merge-iter op (split seq))))

; No.23
(define (prefix xs ys)
  (cond
    ((null? ys) #t)
    ((equal? (car xs) (car ys)) (prefix (cdr xs) (cdr ys)))
    (else #f)))

; No.24
(define (suffix-1 xs ys)
  (prefix (reverse xs) (reverse ys)))

(define (suffix-2 xs ys)
  (prefix (drop xs (- (length xs) (length ys))) ys))

; No.25
(define (sublist xs ys)
  (cond
    ((null? xs) #f)
    ((or (null? ys) (prefix xs ys)) #t)
    (else (sublist (cdr xs) ys))))

; No.26
(define (member-tree x seq)
  (define (flat-tree seq)
    (cond
      ((null? seq) nil)
      ((not (pair? seq)) (list seq))
      (else (append
              (flat-tree (car seq))
              (flat-tree (cdr seq))))))
  (if (member x (flat-tree seq)) #t #f))

; No.27
(define (count-leaf seq)
  (cond
    ((null? seq) 0)
    ((not (pair? seq)) 1)
    (else (+ (count-leaf (car seq)) (count-leaf (cdr seq))))))

; No.28
(define (subst x y seq)
  (cond
    ((null? seq) nil)
    ((not (pair? seq))
     (if (equal? x seq)
       y
       seq))
    (else (cons (subst x y (car seq)) (subst x y (cdr seq))))))

; No.29
(define (permutation seq)
  (cond
    ((null? seq) nil)
    ((null? (cdr seq)) (list seq))
    (else (flatmap
      (lambda (x) (map 
                    (lambda (y) (cons x y))
                    (permutation (difference seq (list x)))))
      seq))))

(define (permutation-n n seq)
  (cond
    ((zero? n) (list nil))
    ((null? seq) (list nil))
    (else (flatmap
      (lambda (x) (map 
                    (lambda (y) (cons x y))
                    (permutation-n (- n 1) (difference seq (list x)))))
      seq))))

; No.30
(define (repeat-perm-1 n seq)
  (cond
    ((zero? n) (list nil))
    ((null? seq) (list nil))
    (else (flatmap
      (lambda (x) (map 
                    (lambda (y) (cons x y))
                    (repeat-perm-1 (- n 1) seq)))
      seq))))

(define (repeat-perm-2 n seq)
  (define (perm-iter n seq p)
    (cond
      ((eq? n 0) p)
      (else
        (perm-iter
          (- n 1)
          seq
          (flatmap
            (lambda (x) (map
                          (lambda (y) (cons x y))
                          p))
            seq)))))
  (perm-iter n seq (list nil)))

; No.31
(define (comb-num-1 n r)
  (define (factorial n)
    (if (zero? n)
      1
      (* n (factorial (- n 1)))))
  (define (perm-num n r)
    (/ (factorial n) (factorial (- n r))))
  (/ (perm-num n r) (factorial r)))

(define (comb-num-2 n r)
  (if (or (zero? r) (equal? r n))
    1
    (* (comb-num-2 n (- r 1)) (/ (+ (- n r) 1) r))))

; No.32
(define (combination n seq)
  (cond
    ((zero? n) (list nil))
    ((null? seq) (list nil))
    (else (flatmap
      (lambda (x) (if (< (length (cdr (member x seq))) (- n 1))
                    nil
                    (map 
                      (lambda (y) (cons x y))
                      (combination (- n 1) (cdr (member x seq))))))
      seq))))

; No.33
(define (repeat-comb n seq)
  (cond
    ((zero? n) (list nil))
    ((null? seq) (list nil))
    (else (flatmap
      (lambda (x) (map 
                    (lambda (y) (cons x y))
                    (repeat-comb (- n 1) (member x seq))))
      seq))))

; No.34
(define (split-nth-1 seq n)
  (values (take seq n) (drop seq n)))

(define (split-nth-2 seq n)
  (define (split seq n front)
    (cond
      ((or (null? seq) (zero? n)) (values (reverse front) seq))
      (else (split (cdr seq) (- n 1) (cons (car seq) front)))))
  (split seq n nil))

; No.35
(define (partition-1 seq)
  (define (odds seq n)
    (cond
      ((null? seq) nil)
      ((not (equal? (remainder n 2) 0)) (odds (cdr seq) (+ n 1)))
      (else (cons (car seq) (odds (cdr seq) (+ n 1))))))
  (define (evens seq n)
    (cond
      ((null? seq) nil)
      ((not (equal? (remainder n 2) 1)) (evens (cdr seq) (+ n 1)))
      (else (cons (car seq) (evens (cdr seq) (+ n 1))))))
  (values (odds seq 0) (evens seq 0)))

(define (partition-2 seq)
  (define (partition-rec seq n odds evens)
    (cond
      ((null? seq) (values (reverse odds) (reverse evens)))
      ((equal? 0 (remainder n 2))
       (partition-rec (cdr seq) (+ n 1) (cons (car seq) odds) evens))
      (else
       (partition-rec (cdr seq) (+ n 1) odds (cons (car seq) evens)))))
  (partition-rec seq 0 nil nil))

; No.36
(define (split-find x seq)
  (define (split-rec x seq front)
    (cond
      ((or (null? seq) (equal? x (car seq)))
       (values (reverse front) seq))
      (else (split-rec x (cdr seq) (cons (car seq) front)))))
  (split-rec x seq nil))

; No.37
(define (split-ge x seq)
  (define (split-rec seq x low high)
    (cond
      ((null? seq) (values (reverse low) (reverse high)))
      ((>= x (car seq))
       (split-rec (cdr seq) x (cons (car seq) low) high))
      (else
       (split-rec (cdr seq) x low (cons (car seq) high)))))
  (split-rec seq x nil nil))

; No.38
(define (pack seq)
  (define (pack-iter seq x xs packed)
    (cond
      ((null? seq) (reverse (cons xs packed)))
      ((equal? x (car seq))
       (pack-iter (cdr seq) x (cons x xs) packed))
      (else (pack-iter seq (car seq) nil (cons xs packed)))))
  (pack-iter seq (car seq) nil nil))

; No.39
(define (pack-num-list seq)
  (define (pnl-iter seq x top packed)
    (cond
      ((null? seq) (if (equal? x top)
                     (reverse (cons top packed))
                     (reverse (cons (cons top x) packed))))
      ((equal? (+ x 1) (car seq))
       (pnl-iter (cdr seq) (car seq) top packed))
      ((equal? x top) 
       (pnl-iter (cdr seq) (car seq) (car seq) (cons top packed)))
      (else
        (pnl-iter (cdr seq) (car seq) (car seq) (cons (cons top x) packed)))))
  (pnl-iter (cdr seq) (car seq) (car seq) nil))

; No.40
(define (expand-num-list seq)
  (define (serialize-iter x end seq)
    (if (equal? x end)
      seq
      (serialize-iter (+ x 1) end (cons (+ x 1) seq))))
  (define (serialize pair)
    (serialize-iter (car pair) (cdr pair) (list (car pair))))
  (define (enl-iter seq expanded)
    (cond
      ((null? seq) (reverse expanded))
      ((pair? (car seq))
       (enl-iter (cdr seq) (append (serialize (car seq)) expanded)))
      (else (enl-iter (cdr seq) (cons (car seq) expanded)))))
  (enl-iter seq nil))

; No.41
(define (encode seq)
  (map (lambda (xs) (cons (car xs) (length xs))) (pack seq)))

; No.42
(define (decode-1 seq)
  (define (serialize-iter x n seq)
    (if (zero? n)
      seq
      (serialize-iter x (- n 1) (cons x seq))))
  (define (serialize pair)
    (serialize-iter (car pair) (cdr pair) nil))
  (define (decode-iter seq decoded)
    (if (null? seq)
      decoded
      (decode-iter (cdr seq) (append decoded (serialize (car seq))))))
  (decode-iter seq nil))

(define (decode-2 seq)
  (define (serialize-iter x n seq)
    (if (zero? n)
      seq
      (serialize-iter x (- n 1) (cons x seq))))
  (define (serialize pair)
    (serialize-iter (car pair) (cdr pair) nil))
  (flatmap serialize seq))

; No.43
(define (any? pred seq)
  (cond
    ((null? seq) #f)
    ((pred (car seq)) #t)
    (else (any? pred (cdr seq)))))

(define (every? pred seq)
  (cond
    ((null? seq) #t)
    ((not (pred (car seq))) #f)
    (else (every? pred (cdr seq)))))

; No.44
(define (maplist-1 proc seq)
  (define (make-list seq maked)
    (if (null? seq)
      (reverse maked)
      (make-list (cdr seq) (cons seq maked))))
  (map proc (make-list seq nil)))

(define (maplist-2 proc seq)
  (if (null? seq)
    nil
    (cons (proc seq) (maplist-2 proc (cdr seq)))))

(define (maplist-3 proc seq)
  (define (maplist-iter proc seq mapped)
    (if (null? seq)
      (reverse mapped)
      (maplist-iter proc (cdr seq) (cons (proc seq) mapped))))
  (maplist-iter proc seq nil))

; No.45
(define (for-each-list-1 proc op initial seq)
  (define (accumulate op seq accumulated)
    (if (null? seq)
      accumulated
      (accumulate op (cdr seq) (op accumulated (car seq) ))))
  (accumulate op (map proc seq) initial))

(define (for-each-list-2 proc op initial seq)
  (if (null? seq)
    initial
    (op (proc (car seq)) (for-each-list-2 proc op initial (cdr seq)))))

; No.46
(define (my-map proc seq)
  (for-each-list proc cons nil seq))

(define (my-filter pred seq)
  (for-each-list
    (lambda (x) (if (pred x) (list x) nil)) append nil seq))

(define (my-accumulate op initial seq)
  (for-each-list (lambda (x) x) op initial seq))

; No.47
(define (Komachi-function)
  (define (divide-Komachi x y z)
    (/ x (+ (* y 10) z)))
  (define (calc-Komachi seq)
    (accumulate
      + 0 (map
            (lambda (xs) (apply divide-Komachi xs))
            (group seq 3))))
  (define (Komachi? seq)
    (and (integer? (/ 1 (calc-Komachi seq)))
         (< (list-ref seq 0) (list-ref seq 3) (list-ref seq 6)))) ;reproduced
  (filter Komachi? (permutation 9 (iota 1 9))))

; No.48
(define (magic-circle)
  (define check-index '((0 1 2) (3 4 5) (6 7 8)
                        (0 3 6) (1 4 7) (2 5 8)
                        (0 4 8) (2 4 6)))
  (define (equal-lines? seq)
    (every? (lambda (x) (eq? x (car seq))) seq))
  (define (make-line seq indexes)
    (map (lambda (x) (list-ref seq x)) indexes))
  (define (safe? seq)
    (equal-lines? (map
                    (lambda (indexes) (apply + (make-line seq indexes)))
                    check-index)))
  (filter safe? (permutation 9 (iota 1 9))))

; No.49

;  W R O N G  (M W R O N G I H T)
; *        M  (0 1 2 3 4 5 6 7 8)
; ----------
;  R I G H T

(define (alphametic)
  (define formula  '(0 1 2 3 4 5))
  (define solution '(2 6 5 7 8))
  (define (calc-word w o r d s)
    (+ (* w 10000) (* o 1000) (* r 100) (* d 10) s))
  (define (calc-formula m w r o n g)
    (* m (calc-word w r o n g)))
  (define (calc-solution r i g h t)
    (calc-word r i g h t))
  (define (select seq indexes)
    (map (lambda (index) (list-ref seq index)) indexes))
  (define (safe? seq)
    (eq? (apply calc-formula  (select seq formula))
         (apply calc-solution (select seq solution))))
  (filter safe? (permutation 9 (iota 1 9))))

; No.50
(define (sieve n)
  (define (prime? n)
    (define (divides? x y)
      (= (remainder y x) 0))
    (define (find-divisor n test-divisor)
      (cond
        ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
    (define (smallest-divisor n)
      (find-divisor n 2))
    (= n (smallest-divisor n)))
  (define (sieve-iter x primes)
    (cond
      ((> x n) (reverse primes))
      ((prime? x) (sieve-iter (+ x 1) (cons x primes)))
      (else (sieve-iter (+ x 1) primes))))
  (sieve-iter 2 nil))

; No.51
(define (xor p q)
  (or (and (not p) q) (and p (not q))))

; No.52
(define (half-adder p q)
  (values (xor p q) (and p q)))

; No.53
(define (full-adder-1 p q r)
  (values
    (or (and p q) (and (or p q) r))
    (xor (xor p q) r)))

(define (full-adder-2 p q r)
  (receive (a b) (half-adder p q)
    (receive (c d) (half-adder a r)
      (values c (or b d)))))

; No.54
(define (int->uint int n)
  (let iter ((int int) (n n) (uint nil))
    (if (= n 0)
      uint
      (iter (quotient int 2) (- n 1) (cons (odd? int) uint)))))

(define (uint->int uint)
  (let iter ((uint uint) (n (- (length uint) 1)))
    (cond ((< n 0) 0)
          ((car uint) (+ (expt 2 n) (iter (cdr uint) (- n 1))))
          (else (iter (cdr uint) (- n 1))))))

; No.55
(define (uint-and xs ys)
  (map (lambda (x y) (and x y)) xs ys))

(define (uint-or xs ys)
  (map (lambda (x y) (or x y)) xs ys))

(define (uint-xor xs ys)
  (map (lambda (x y) (xor x y)) xs ys))

(define (uint-not xs)
  (map (lambda (x) (not x)) xs))

; No.56
(define (uint-add xs ys)
  (cond ((or (null? xs) (null? ys))
         (values nil #f))
        (else
          (receive (seq r) (uint-add (cdr xs) (cdr ys))
          (receive (s c) (full-adder (car xs) (car ys) r)
                   (values (cons s seq) c))))))

; No.57
(define (uint-inc xs)
  (cond ((null? xs) (values nil #t))
        (else
          (receive (seq r) (uint-inc (cdr xs))
          (receive (s c) (full-adder (car xs) #f r)
                   (values (cons s seq) c))))))

; No.58
(define (uint-sub xs ys)
  (receive (complement+1 crry) (uint-inc (uint-not ys))
    (receive (result crry) (uint-add xs complement+1)
             (values result (not crry)))))

; No.59
(define (uint-srl xs)
  (define (shift xs result)
    (if (null? (cdr xs))
      (values (reverse result) (car xs))
      (shift (cdr xs) (cons (car xs) result))))
  (receive (shifted lsb) (shift xs nil)
           (values (cons #f shifted) lsb)))

(define (uint-sll xs)
  (values (append (cdr xs) (list #f))
          (car xs)))
