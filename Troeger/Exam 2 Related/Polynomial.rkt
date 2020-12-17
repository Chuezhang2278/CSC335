;'((3 * (x ^ 2)) + (1 * (x ^ 1)) + (2 * (x ^ 0)))

;(leading-coefficient degree rest-of-polynomial)

;((3 * (x ^ 2)) + (1 * (x ^ 1)) + (2 * (x ^ 0))) would be represented (3 2 p1), where p1 = (1 1 p2) is the representation of ((1 * (x ^ 1)) + (2 * (x ^ 0)))
; and p2 = (2 0 '()) is the representation of ((2 * (x ^ 0))). Note that the degree of rest-of-polynomial is less than that of polynomial. 
; The zero polynomial is represented (0 0 '()).  Expanding the subterms, one has (3 2 (1 1 (2 0 '()))) as the representation
; of ( (3 * (x ^ 2)) + (1 * (x ^ 1)) + (2 * (x ^ 0)) )

(define (make-poly coeff degree rest-of-poly)
  (cond ((null? rest-of-poly) (list (list coeff '* (list 'x '^ degree))))
        (else (append (list (list coeff '* (list 'x '^ degree)) '+) rest-of-poly))))

(define p2 (make-poly 2 0 '()))
(define p1 (make-poly 1 1 p2))
(define p0 (make-poly 3 2 p1))

;p0

; Problem 2a (15 points, with 10 points for development and proof)
; Design, implement and certify a procedure rep-to-poly which inputs a representation r of a polynomial p, as defined above, and which returns
; the polynomial p, as a list.  For example, (rep-to-poly '(3 2 (1 1 (2 0 '())))) will return ( (3 * (x ^ 2)) + ( 1 * (x ^ 1)) + (2 * (x ^ 0)) ).
; Working test cases are required.

; A key design goal for 2a and 2b is that the composition of poly-to-rep and rep-to-poly should yield the appropriate identity function.
(define y '(3 2 (1 1(2 0'()))))

(define (last-element lis)
  (cond ((null? (cdr lis)) (car lis))
        (else (last-element (cdr lis)))))

(last-element '(1 1 (2 0 '())))
(cadr '(2 0 '()))

(define (rep-to-poly lis)
  (cond ((null? (last-element lis)) (list (list (car lis) '* (list 'x '^ (cadr lis)))))
        (else (append (list (list (car lis) '* (list 'x '^ (cadr lis))) '+) (rep-to-poly (car (cddr lis)))))))

(rep-to-poly y)