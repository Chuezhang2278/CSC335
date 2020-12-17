(define (sort lis)
  (cond ((null? lis) lis)
        ((null? (cddr lis)) lis)
        ((> (car lis) (cadr lis)) (cons (cadr lis) (sort (append (list (car lis)) (cddr lis)))))
        ((< (car lis) (cadr lis)) (cons (car lis) (sort (cdr lis))))))

(sort '(5 8 9 10 20))

(define (bubblesort lis)
  (define (main lis sorted_lis)
    (cond ((null? lis) sorted_lis)
          ((null? (cddr lis)) sorted_lis)
          ((> (car lis) (cadr lis)) (main (sort sorted_lis) (sort sorted_lis)))
          (else (main (cdr lis) sorted_lis))))
  (main lis lis))

(bubblesort '(5 10 9 8 7))


         
         