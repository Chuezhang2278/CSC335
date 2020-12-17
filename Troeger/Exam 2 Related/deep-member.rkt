; Problem 3a  (10 points).  Write a deeply recursive version of the generalized member? function, that is, your function deep-member?
; ought to take both a predicate and a tree as arguments, and return #t if the tree contains a subtree for which the predicate is true, and #f otherwise.
; For example,

;(deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))

; should return #t. NOTE that I am not requiring a proof for 3a.

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (deep-member? pred tree)
  (cond ((null? tree) #f)
        ((atom? tree) (pred tree))
        (else (or (pred (car tree))
                  (deep-member? pred (car tree))
                  (deep-member? pred (cdr tree))))))

(define (deep_member? pred tree)
  (cond ((null? tree) #f)
        ((atom? tree) (pred tree))
        (else (or (pred (car tree))
                  (deep-member? pred (car tree))
                  (deep-member? pred (cdr tree))))))
        

(deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))