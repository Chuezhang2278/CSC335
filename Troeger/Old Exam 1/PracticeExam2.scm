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
;;;; Problem 1 - synergy: proof and code developed at the same time (out of 10 points)  -- 4: you've made a start

;;;; Problem 2 - code (out of 10 points)  8
;;;; Problem 2 - proof (out of 10 points)    2
;;;; Problem 2 - synergy: proof and code developed at the same time (out of 10 points)   8

;;;; Problem 3 - code (out of 10 points)  
;;;; Problem 3 - proof (out of 15 points)
;;;; Problem 3 - synergy: proof and code developed at the same time (out of 10 points)

;;;; Problem 4 - code (out of 10 points)

;;;; Total    
;;;; Letter Grade  


;;;; Additional comments behind ;;;; in your responses




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

;;;; INSERT YOUR ANSWER HERE
; Synergy:

; I think that I will be inducting on the digits of n, because I need to figure out a way to find and extract the largest digit within n
; then find and extract the smallest digit within n and find the difference between the largest and smallest, which is what the program will ultimately be returning at the end.
; So integer n has x amount of digits, I will intend to prove that when you call (DigitSpread n) what will be returned it LargestD - SmallestD.

;
; We could also create a wrapper that'll cover the case of negative integers (abs n)

(define (digitSpread n)
  (define (digitSpread-aux LeftDig LargestDig SmallestDigit)
    (cond ((zero? LeftDig) (- LargestDig SmallestDig))
          ((> (modulo LeftDig 10) LargestDig) (digitSpread-aux (quotient LeftDig 10) LeftDig SmallestDig))
          ((< (modulo LeftDig 10) SmallestDig) (digitSpread-aux (quotient LeftDig 10) LargestDig LeftDig))

          ;;;; PROBLEM : you forgot to close off the parens, and then also the body of the digitSpread function.
          )) 

  2)  ;;;; I added three parens and the 2 so I could build the rest of your exam.  


          ;;;; PROBLEM :  This is iterative!

          ;;;; Can you figure out how to take the same idea and make it work in a recursive solution?
         

; Problem 2 (30 points) Write and certify an iterative scheme procedure count-digits which inputs a single
; non-negative integer n and which returns the number of digits in n.  Your proof should be based on an
; invariant.  Again, do not forget the termination argument.   


;;;; INSERT YOUR ANSWER HERE
; Synergy:

; So from the start I think I am inducting on the number of digits in non-negative integer n

;;;; right

; I would go about this problem using the predefined function quotient to reduce n by 1 digit and every time I reduce n, I update it.
; So I would create a new variable number-so-far and initialize it with n.


;;;; this is backwards -- n should be the initial value of something like number-remaining.  Try to choose your variable names to accurately reflect their role in
;;;; your progam.  



; Then in the iterative body it will be updated after each iteration. Moreover,
; every time I reduce number-so-far by 1 digit I would increment a variable, numbers-processed which will be initialized 0, by 1. So
; Test Cases
; i.e n=123
; (quotient number-so-far(123) 10) = 12  == number-so-far = 12 === digits-processed = 1
; (quotient number-so-far(12) 10) = 1    == number-so-far = 1  === digits-processed = 2
; (quotient number-so-far(1) 10) = 0     == number-so-far = 0  === digits-processed = 3
; (zero? number-so-far) digits-processed, so here I reached a base case -- by using quotient to reduce n I will reach 0
;  by the time I reach zero digits-processed will contain the number of digits in integer n, so that's what the program returns upon termination.


;;;; you clearly have the right idea.  I just wish you had paid more attention to the lackof alignment between your basis case and the argument you are proposing



; Prototype:
;(define (num-digits n) (num-digits-iter n 0))
;        (define (num-digits-iter number-so-far digits-processed)
;      (cond ((zero? number-so-far) digits-processed) ;  base-case
;             (else (let (quotient number-so-far 10) number-so-far ;; but then we'll immediately be starting with a reduced n...
;                     (quotient number-so-far 10) (+ digits-processed 1))))

; Proof:
; I am inducting on the number of digits in non-negative integer n, because after each iteration I am counting how many digits n contains.
; Throughout that process my variables: number-so-far and digits-so-far are changing. So I assume that my guess-invariant is that with each iteration
; number-so-far will eventually reach zero, while digits-processed will return the total number of digits in n.

; Assuming that my preconditions, n is a non-negative integer, is met then my program will stop once number-so-far reaches zero.
; Notice that quotient will reduce the number by 10 recursively therefore the number has to reach 0, terminating the recursion.


;;;; HERE is where you blow it: the code is iterative, and even though your discussion is based on some invariant-sounding ideas, you go ahead and
;;;; describe an argument which is not invariant-based.  It is sort of hybrid ...


;;;; we can talk about this in office hours -- I am sure you will get it. 



; Final Code:
(define (count-digits n) (if (= n 0) 1
                             (count-digits-aux n 0)))
        (define (count-digits-aux number-so-far digits-processed)
      (cond ((zero? number-so-far) digits-processed) ;  base-case
             (else (count-digits-aux (quotient number-so-far 10) (+ digits-processed 1)))))



;;;; PROBLEM:  (count-digits 0) should be 1, not 0




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

;;;; INSERT YOUR ANSWER HERE
; Synergy:

; Count-digits procedure allowed my to... count the digits of the non-negative input n. However in the program we just increment a counter that is
; keeping track of how many digits are in n. So perhaps we use count-digits ability to separate the numbers and pass those numbers as arguments to
; procedure f, where that procedure will be able to perform whatever it does (i.e square)(define (count-digits n) (count-digits-aux n 0))

;Code
 (define (scramble f n)
   (define (count-digits n) (count-digits-aux n 0))
        (define (count-digits-aux number-so-far digits-processed)
      (cond ((zero? number-so-far) digits-processed) ;  base-case
             (else (count-digits-aux (quotient number-so-far 10) (+ digits-processed 1)))))
   (define scramble-aux 


;;;; not complete, and indeed, nothing useful for the new design has been written.  Note that there was no need to repeat the definition of count-digits here.  




     
     
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



;;; make sure you know how to do this!































