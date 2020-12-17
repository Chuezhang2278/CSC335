;get second element of each inner pair

(define x (list (list 1 2) (list 3 4) (list 5 6 7)))

(define (get-2nd lis count)
  (cond ((null? lis) lis)
        ((= count 1) (car lis))
        (else (get-2nd (cdr lis) (+ count 1)))))

(define (main lis)
  (cond ((null? lis) lis)
        (else (cons (get-2nd (car lis) 0) (main (cdr lis))))))

(main x)

