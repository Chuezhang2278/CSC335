; Problem 4a.  (10 points) Write and prove correct a pair of MUTUALLY RECURSIVE programs odd-indexed-elements and
; even-indexed-elements to produce from one list L the two lists consisting, respectively, of the elements of L with
; even index and the elements of L with odd index.  For example, (odd-indexed-elements '(a b c d e f g h)) = '(b d f h)
; and (even-indexed-elements '(a b c d e f g h)) = '(a c e g).  (Hint: use one proof for both functions).  


(define (even-indexed-elements lis)
  (cond ((null? lis) lis)
        (else (cons (car lis) (even-indexed-elements (cddr lis))))))

(even-indexed-elements '(a b c d e f g h))

(define (odd-indexed-elements lis)
  (cond ((null? lis) lis)
        (else (cons (cadr lis) (odd-indexed-elements (cddr lis))))))

(odd-indexed-elements '(a b c d e f g h))
  