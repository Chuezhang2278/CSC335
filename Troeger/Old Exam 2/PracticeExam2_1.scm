; CSc 335
; Fall 2018

; November 15

; Second Midterm Exam - 2 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 25 points)
;;;; Problem 1 - proof, including synergy (out of 25 points)

;;;; Problem 2 - code (out of 10 points)
;;;; Problem 2 - proof, including synergy (out of 10 points)

;;;; Problem 3 - code (out of 10 points)

;;;; Problem 3b - code (out of 10 points)
;;;; Problem 3b - proof, including synergy (out of 10 points)
 


;;;; Total
;;;; Letter Grade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework:

; no lists, no vectors, no strings, no assignment...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems. 


; Problem 1 (50 points).  A simple recursive idea for finding all prime factors of a positive integer n can be described as follows:

;;     prime-factors(1) = {} (the empty set)
;;     prime-factors(p) = {p} if p is prime
;;     prime-factors(n) = prime-factors(a) U prime-factors(b), where n = ab

; Using this approach, develop a complete R5RS program prime-factorization which inputs an
; integer n >= 1 and returns a list of pairs of the form <prime, power>,
; where prime occurs power times in the prime factorization of n.  Thus

;;    (prime-factorization 1) = ()
;;    (prime-factorization 7) = ((7 1))
;;    (prime-factorization 30) = ((2 1) (3 1) (5 1))
;;    (prime-factorization 100) = ((2 2) (5 2))
;;    (prime-factorization 1024) = ((2 10))

(define (prime-factors number divisor)
  ( number = 1 return number)
  (divisor * divisor > number return number)
  (if (number % divisor = 0 then cons it in and increment divisor )
      (increment divisor by 1))
  divisor = 2)

(define (filter x lis)
  x = element we are looking for in the lis
  (cons into a lis)
  (x, len(filter x lis)))

(define (remmove x lis)
  cons anything but x

(main lis)
  (null? lis)
  (cons (filter(prime-factors) (main (remove lis))))


; The output list must be sorted by primes, with smaller prime numbers appearing before
; larger prime numbers.

; In addition,
;
;   the factors a and b of n must be chosen as close in size to each other as possible (for 200, for example, a = 10 and b = 20)
;
;   your code must be purely functional.
;
;   complete proofs of your major functions must be included.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


              
; Problem 2 (20 points).  Is an alternative approach to the problem of finding all prime factors of a positive integer n superior to the
; one described in Problem 1?  Give a complete solution making use of enumerate-interval and for which the primary function uses map.
; Your main functions should be proved correct, but proofs given for Problem 1 (eg, for the function prime?) need not be repeated.

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 3a  (10 points).  Write a deeply recursive version of the generalized member? function, that is, your function deep-member?
; ought to take both a predicate and a tree as arguments, and return #t if the tree contains a subtree for which the predicate is true, and #f otherwise.
; For example,

;(deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))

; should return #t. NOTE that I am not requiring a proof for 3a.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 3b.  (20 points) Write and prove correct a procedure remove-left-most which inputs a predicate and a tree, and which removes only the left-most subtree
; of the input tree for which the predicate is true.  For example,

; (remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e)))))))
;
; =
;
; '((a (b c)) ((f (g (c (d e))))))


; Hint: use your deep-member? function.


