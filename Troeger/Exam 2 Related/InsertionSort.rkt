(define (sort x lis)
  (cond ((null? lis) (list x))
        ((> (car lis) x) (cons x lis))
        (else (cons (car lis) (sort x (cdr lis))))))

(define (insertionsort lis)
  (define (insert lis ret)
    (cond ((null? lis) ret)
          (else (insert (cdr lis) (sort (car lis) ret)))))
  (insert lis '()))

(insertionsort '(1 9 7 2 3))

; Sort
; Pre-Condition: non empty list
; post-condition: sorted list

; Insert
; Pre-Condition: non empty list
; Post-condition: Sorted list

