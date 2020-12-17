(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))

(define (flat lis)
  (cond ((null? lis) lis)
        ((pair? lis) (append (flat (car lis)) (flat (cdr lis))))
        (else (list lis))))

(define (to-one lis)
  1)

(define (main lis)
  (define temp (map to-one (flat lis)))
  (accumulate + 0 temp))

(main '(a b (c d ( g h))))

