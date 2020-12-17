

; CSc 335
; Fall 2015

; October 13

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 15 points)      na
;;;; Problem 1 - proof (out of 10 points)     na

;;;; Problem 2 - code (out of 10 points)       9
;;;; Problem 2 - proof (out of 15 points)     10

;;;; Problem 3 - code (out of 20 points)       5
;;;; Problem 3 - proof (out of 20 points)     na

;;;; Problem 4 (out of 10 points)             na

;;;; Total                                    24
;;;; Letter Grade                              D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework: no lists, no strings, no assignment.  

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  


; Problem 1 (25 points) Write a scheme function expo of one integer argument b which returns a function of
; one argument e so that, for e a non-negative integer, ((expo b) e) returns b raised to the eth power.
; The function returned by (expo b) should work by repeated multiplication, and should be be properly
; recursive; give a proof that this function is correct.  Don't forget the termination argument.

; CODE:

(define expo
  (lambda (b)
    (lambda (e)
      (cond
        ((= e 0) 1)
        (else (* b ((expo b) (- e 1))))))))

((expo 2) 4)

;;;; nothing attempted



; Problem 2 (25 points) Design and certify an iterative scheme procedure count-digits which inputs a single
; non-negative integer n and which returns the number of digits in n.  Your proof should be based on an
; invariant.  Again, do not forget the termination argument.   

; CODE:

;;;; one problem:  0 has one digit, not zero digits



; tests
(display "QUESTION 1\n")
;(count-digits 123)
;(count-digits 1000)

; PROOF:
; (cd-iter 1234 0)
; (cd-iter 123 1)
; (cd-iter 12 2)
; (cd-iter 1 3)
; (cd-iter 0 4)
; 4

; GUESS INVARIANT: count = the number of digits deleted from n


;;;; this is ok



; Before the first iteration it is clear that count is 0 and no digits were deleted
; Before the last call, count = n, exactly yhe number of digits deleted from n.


;;;; you omit the most important part of the argument -- showing that your guess invariant really is maintained
;;;; invariant




; TERMINATION ARGUMENT: 
; we see that each time cd-iter is called, it divides n by 10 (and truncates it.)
; Thus n will eventually become 0, and cd-iter will return count.











; Problem 3 (40 points)  Write and certify a scheme function scramble with arguments n and f, where n is a
; positive integer and f is a function from the set {0,1,2,...,9} of digits to the set of non-negative
; integers, and which returns the number formed from n by replacing each digit j by the digits (in order)
; of the value (f j).

; Thus if f is the function which squares each digit, (scramble 403612 f) returns 16093614

; Your function can be either recursive or iterative, as you see fit: be sure to say which, and to give
; a proof (induction or invariant based) which matches your choice. Again, don't forget the termination argument.

; (Hint: work from the right, and perhaps make use of your function count-digits. Your proof, should you use
; count-digits, will need to show that the count-digits precondition holds each time it is called; your proof should also
; indicate how the post-condition of count-digits contributes to the main argument. )

(display "QUESTION 3\n")
; CODE:
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (find-div n count)
  (cond
    ((zero? n) (expt 10 (- count 1)))
    (else (find-div (quotient n 10) (+ count 1)))))
(find-div 123 0)

; This is RECURSIVE
(define (scramble f n)
  (scramble-rec f n (find-div n 0)))

(define (scramble-rec f n div)
  (cond
    ((= div 0) 0)
    (else (+ (* (f (quotient (- n (remainder n div)) div)) div)
             (scramble-rec f (remainder n div) (quotient div 10))))))
  
; tests
(scramble square 123)


;;;; does not work: (scramble square 5432) returns 26694

;;;; you need to look at how you build up the new number

;;;; even a partial proof would have elucidated your design idea -- eg, what is the design role of div?











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
; power (you can use the scheme primitive, expt).

; No proofs are required for problem 4.

((add-x 2) 4)

;;;; nothing written


;; not sure if this is right

(define curry-2
  (lambda (f)
    (lambda (x)
      (lambda (y)
      (f x y)))))

(((curry-2 expt) 2) 5)
(((curry-2 +) 2) 2)



















