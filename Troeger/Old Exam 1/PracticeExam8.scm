

; CSc 335
; Fall 2017

; October 10

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 10 points)  10
;;;; Problem 1 - proof (out of 10 points   5
;;;; Problem 1 - synergy: proof and code developed at the same time (out of 10 points)  5

;;;; Problem 2 - code (out of 15 points)  12
;;;; Problem 2 - proof (out of 15 points)   7
;;;; Problem 2 - synergy: proof and code developed at the same time (out of 10 points)  9

;;;; Problem 3 - code for sigma (out of 10 points)  0
;;;; Problem 3 - code for approximation function (out of 20 points)   5



;;;; Total   53
;;;; Letter Grade   B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework: no lists, no strings, no assignment.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  


; Problem 1 (30 points) Write and certify an iterative procedure decompose-as-sum-of-squares (dss, for short) which inputs a positive integer n
; and which outputs the first integer p such that (i) p^2 + q^2 = n, where q^2 = (n - p^2), and (ii) neither p nor n - p^2 is 1.
; If such p does not exist, the function should return n.   Thus, noting that 4^2 + 7^2 = 16 + 49 = 65, 

;        (dss 65)

; should return 4, and

;        (dss 50)

; should return 5 (as 25 + 25 = 50).  

; But

;        (dss 2)

;  should return 2.


; Note that (= (square (sqrt 2.0)) 2) returns #f, as does (= (square (sqrt 2.0)) 2.0 )-- you will need to find another way of
; testing whether a number is a perfect square (hint: use a loop without using sqrt).  

; Each loop you write needs certification; do not forget the termination arguments. 

;;;; INSERT YOUR ANSWER HERE
(define (square x) (* x x))

(define (dss n)
  (dss-iter n 2))

(define (dss-iter n p)
  (cond ;((= p 1) 0)
        ;((= (- n p) 1) 0)
        ((= p n) n)
        ((= n (+ (square p) (square (- n p)))) p ) ; wont call??
        (else (dss-iter n (+ p 1)))))



;;;; this code is correct for the first version of the problem, which I corrected right at the start of the exam.  I can give you full code credit,
;;;; and credit for your invariant (since it is a good one), but not for the rest of the proof / synergy.  

; proof

; Guess Invariant: All values of p does not satisfy (p^2)+(p-q)^2 = n so far.
; First test: The first call of the program will 



; Problem 2 (40 points) Write and certify a recursive procedure for computing the sum of the digits occurring in a nonnegative integer.  Your procedure
; must not be a linear recursion - instead, like quicksort, it needs to reduce the amount of work remaining by half in a constant number of calls.

; In other words, what I want is a procedure which, on the assumption that the scheme primitives work in constant time, computes the sum of the digits
; in n in time proportional to the log of the number of digits in n.

; Again, do not forget the termination argument, in this case enhanced to include support for the claim that your program runs in log time.  Can you
; give the run-time estimate in terms of n, as well as in terms of the number of digits in n?  



;;;; INSERT YOUR ANSWER HERE

(define (sod n)
  (let ((curr (remainder n 10)))
    (cond ((= curr 0) 0)
          (else (+ curr (sod (quotient n 10)))))))


;;;; this is a standard linear recursion ... 


;proof
; Pre-conditions: n is an integer and >= 0;
; Post-condition: (sod n) returns the sum of digits in n as an integer

; Basis Step: When n is 0, the program returns 0 through the condition ((= curr 0) 0)
; Induction Hypo: Assume that (sod n) will return the sum of the digits in n
; Induction Step: Since (sod n) will return the sum of digits in the nth call, the (n+1)th call of the program will return
;    the sum of the current digit ((n+1)th digit) and (sod n). (+ curr (sod (quotient n 10)))
; Termination Step: The program terminates when the curr is 0. This happens because n is being decreased by powers of ten
;    every call. (quotient n 10)

;;;;;;;;;;;;;;;;;;;; log time attempt

(define (count-dig n result)
  (let ((curr (remainder n 10)))
    (cond ((= curr 0) result)
          (else (count-dig (quotient n 10) (+ result 1))))))

;(define (sum-log n)
;  (let ((curr (quotient n (count-dig(n 0)))))
;  (cond ((= (count-dig n) 0) 0)
;        (else (+ (sod (quotient n (* 10 (/ (count-dig(n 0)) 2))) (sod (remainder n (* 10 (/ (count-dig(n 0)) 2)))))))))) left and right recursion attempt




;;;; good start!  the idea is to set up a recursion which is reminiscent of quicksort -- a tree recursion, which guarantees that the calling stack height will never be more than log(dign), where dign is the number of digits in n.

;;;; I was careless in jumping to the statement that the overall run time for the procedure would be log(dign), however: in the end, O(dign) additions need to be carried out -- the thing ends up being linear.  



; Problem 3 (30 points) The function

; It is known that for 0 <= x < 1, -log(1 - x) can be approximated as the sum of terms (x^k)/k, with k running from 1 up to n,
; The approximation improves as n increases.  Write a function log-approx of one argument, n, where n is a positive integer,
; such that ((log-approx n) x) gives the value resulting from computing the first n terms of this series.

; Some known values, to 6 significant figures:
;  - log(1 - 0.25) = 0.287682
;  - log(1 - 0.5) = 0.693147
;  - log(1 - 0.75) = 1.38629

; Thus, you would expect ((log-approx 100) 0.75) to be a number close to -1.38629.

; NO PROOF IS REQUIRED FOR PROBLEM 3




;;;; PUT YOUR ANSWER HERE
(define log-approx
  (lambda (n)
    (lambda (x)
      (log n x))))

;;;; arghhhh.... why would I want you to use the log function here?   More: where do you define a binary log function?   I can give you some points for forming the closure this way. 

(define (log-it n x)
  (log-iter n x 1 0))

(define (log-iter n x k result)
  (cond ((= k n) result)
        (else (log-iter (n x (+ k 1) (+ result (/ (expt x k) k)))))))
  

 ;;;; not what is wanted.  































