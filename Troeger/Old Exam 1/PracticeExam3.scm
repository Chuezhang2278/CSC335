

; CSc 335
; Spring 2017

; March 9

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 8 points)  
;;;; Problem 1 - proof (out of 7 points
;;;; Problem 1 - synergy: proof and code developed at the same time (out of 10 points)  3  : you've made a start, albeit iterative. 

;;;; Problem 2 - code (out of 10 points)  10
;;;; Problem 2 - proof (out of 10 points)  10
;;;; Problem 2 - synergy: proof and code developed at the same time (out of 10 points)  10

;;;; Problem 3 - code (out of 10 points)  8
;;;; Problem 3 - proof (out of 15 points)  1
;;;; Problem 3 - synergy: proof and code developed at the same time (out of 10 points)  0

;;;; Problem 4 - code (out of 10 points)  0

;;;; Totalt  42
;;;; Letter Grade  C-

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


; Problem 1 (25 points) Write and certify a properly recursive scheme function DigitSpread of one integer
; argument n which returns the difference between the largest and smallest digits occurring in n.  Don't forget the termination argument.

;
;(define (digitspread n)
;  (define (iter num ma mi)
;    (if (= num 0) (- ma mi))
;
;    (cond (and (> (remainder num 10) max) (iter (quotient num 10)
;    (if (> (remainder num 10) (remainder (quotient num 10) 10))
;        (iter (quotient num 10) (remainder num 10) (remainder (quotient num 10) 10))
;    (iter (quotient num 10) (remainder (quotient num 10) 10) (remainder num 10))))
;
;  (iter n 
;


;;;; INSERT YOUR ANSWER HERE
(define (digitspread n)
  (define (iter num ma mi)
   ;(if (= num 0) (- ma mi) 0)

    (cond ((= num 0) (- ma mi))
           ((> (remainder num 10) ma) (iter (quotient num 10) (remainder num 10) mi))
           ((< (remainder num 10) mi) (iter (quotient num 10) ma (remainder num 10)))
           (else (iter (quotient num 10) ma mi))))

  (iter n (remainder n 10) (remainder n 10)))


(digitspread 12345)
(digitspread 1234)
(digitspread 12)
(digitspread 1)
(digitspread 990099)
;WORKSSS!!!! BUT NO TIME TO PROVE :(


;;;; and ... your code is iterative, when a recursive form was requested.







; Problem 2 (30 points) Write and certify an iterative scheme procedure count-digits which inputs a single
; non-negative integer n and which returns the number of digits in n.  Your proof should be based on an
; invariant.  Again, do not forget the termination argument.   




;;;; INSERT YOUR ANSWER HERE
(define (count-digits n)
  (define (counter rest-num result)
    (if (= rest-num 0) result
    (counter (quotient rest-num 10) (+ 1 result))))

  (if (= n 0) 1
   (counter n 0)))
    
;Example
;(counter 123 0)
;(counter 12 1)
;(counter 1 2)
;(counter 0 3)

;**CERTIFY**
;To certify the correctness of our program we come out with the following guess invariant:
; Total Digits in n = Digits in rest-num + result
;In other words, the total number of digits equals the result so far plus the unprocessed part of the number


;;;; good


;We check our guess invariant with the 3 tests:

;[1] Does it work correctly at the end of the program?
;When the program ends (= rest-num 0) is true and the result is returned. This means that all numbers have been counter
;and result contains the digit count correctly. We have Total Digits in n = 0 + result. This holds the guess invariant. 

;[2] Does it work in the first call?
;During the first call to (counter rest-num result) , result is 0 and rest-num is the original number n.
;This means no digits have been counted. So the guess invariant holds. Total Digits in n = Digits in n + 0

;[3] If it works in one call, does it work in the next?
;In a kth call assume (counter rest-num result) hold the invariant Total Digits in n = Digits in rest-num + result
;The kth call returns the k+1th call where we have (counter (quotient rest-num 10) (+1 result)). This means we cut the last digit
;from rest-num and add one to the result. Therefore, the guess invariant hold since Total Digits in n = (Digits in rest-num -1) + (result + 1)

;[*Termination argument: Since rest-num is a nonegative integer, it is being integer divived by 10 in every call at some point
;rest-num will become 0 and this terminates the program. 




; Problem 3 (35 points)  Write and certify a scheme function scramble with arguments n and f, where n is a
; positive integer and f is a function from the set {0,1,2,...,9} of digits to the set of non-negative
; integers, and which returns the number formed from n by replacing each digit j by the digits (in order)
; of the value (f j).

; Thus if f is the function which squares each digit, (scramble 403612 f) returns 16093614

; Your function can be either recursive or iterative, as you see fit: be sure to say which, and to give
; a proof (induction or invariant based) which matches your choice. Again, don't forget the termination argument.

; (Hint: work from the right, and perhaps make use of your function count-digits. Your proof, should you use
; count-digits, will need to show that the count-digits precondition holds each time it is called; your proof should also
; indicate how the post-condition of count-digits contributes to the main argument. )

(define (ex b n)
  (if (= n 0) 1
   (* b (ex b (- n 1)))))

(define (sq x) (* x x))

(define (scramble n f)
  (define (iter num counter result)
   (if (= num 0) (quotient result 10) ;Readjust numbers (remove zero at the end)
   (iter (quotient num 10) (+ counter 1) (+ result (* (ex 10 (count-digits result)) (f(remainder num 10)))))))

  (iter n 0 0))


;;;; everything works except embedded 0s -- eg, (scramble 2094 sq) returns 48116, and (scramble 100 sq) returns 1


;;;; INSERT YOUR ANSWER HERE

;To certify we have the foollowing guess invariant:
; result * 10 = 10^counter 


;;;; incomplete ...


; Problem 4 (10 points) The function

(define add-1
  (lambda (y) (+ y 1)))

; can be generalized

(define add-x
  (lambda (x)
    (lambda (y)
      (+ x y))))

; so that add-1 can be realized as (add-x 1).

; In this problem, you are asked to go a step further, and generalize this pattern to allow its use with any function
; of two arguments (not just +).  Name the new function curry-2, and show how to use it to realize add-x.  Show
; further how to use it to define a function expo-b, which inputs a non-negative integer e and outputs b raised to the eth
; power (you may use the scheme primitive, expt).

; NO PROOFS ARE REQUIRED FOR PROBLEM 4




;;;; PUT YOUR ANSWER HERE
































