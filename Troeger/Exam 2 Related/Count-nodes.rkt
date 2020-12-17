(define (count-nodes tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (count-nodes (car tree)) (count-nodes (cdr tree))))))

(define tree (list (list 1 (list (list 2 2 3) 2)) (list (list (list 1 2) (list 2 2 3) 3) 4 4 9)))
(count-nodes tree)