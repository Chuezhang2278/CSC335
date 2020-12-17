(define tree (list (list 1 (list (list 2 2 3)) ) (list (list (list 1 2) (list 2 2 3) 3) 4 4 9)))

(define (has-list? lis)
  (cond ((null? lis) lis)
        ((integer? lis) #f)
        ((list? (car lis)) #t)
        (else (has-list? (cdr lis)))))

(define (visual tree)
  (display tree)
  (display "\n")
  (cond ((null? tree) tree)
        ((has-list? tree) (visual (cdr (car tree))))
        (else (visual (cdr tree)))))
        
(visual tree)

(define (to-list tree)
  (display tree)
  (display "\n")
  (cond ((null? tree) tree)
        ((has-list? tree) (to-list (cdr (car tree))))
        (else (to-list (cdr tree)))))