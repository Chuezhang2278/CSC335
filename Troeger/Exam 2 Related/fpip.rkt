;Pre-condition : s-exp , varlst
;Post-condition : #t or #f

(define variables '(a b c d e f g))

(define (fpip s-exp varlst)
  (define (member? x lst)
  (cond ((null? lst) #f)
        ((eq? x (car lst)) #t)
        (else (member? x (cdr lst)))))
  (define (flatten x)
    (cond ((null? x) '())
          ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
          (else (list x))))
  (define (main lst)
    (cond ((null? lst) #t)
          ((member? (car lst) variables) (main (cdr lst)))
          ((or (eq? (car lst) '+)(eq? (car lst) '-)) (main (cdr lst)))
          (else #f)))
  (main (flatten s-exp)))

(define test '((a + (b + c)) + ((d + (e + g)) + h)))

(fpip test '())

