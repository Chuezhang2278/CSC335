; CSc 335
; Fall 2018

; October 11

; First Midterm Exam - 2 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 15 points)
;;;; Problem 1 - proof (out of 5 points)
;;;; Problem 1 - synergy: connection between proof and code (out of 5 points)

;;;; Problem 2 - code (out of 15 points)
;;;; Problem 2 - proof (out of 8 points)
;;;; Problem 2 - synergy: connection between proof and code (out of 7 points)

;;;; Problem 3 - code and specification (out of 10 points)

;;;; Problem 4 - code (out of 20 points)
;;;; Problem 4 - proof (out of 8 points)
;;;; Problem 4 - synergy (out of 7 points)



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

; Problem 1a (15 points) Write a Scheme function quo of one integer argument d which returns a function of one integer
; argument n so that ((quo d) n) is the quotient on dividing n by d.  For example, ((quo 4) 6) = 1, as 4 divides 6 just one time.  
; Assume n >= 0 and d > 0.  The function returned by (quo d) should be properly recursive, and should work by repeated subtraction.


; Problem 1b (10 points, including synergy) Prove by induction that the call (quo d) of your function quo returns a function which actually
; does compute the quotient on dividing n by d, assuming n >= 0 and d > 0.  Don't forget to include a termination argument. 


; Problem 2a (15 points) A positive integer n > 1 is said to be perfect if it is the sum of its proper divisors. Thus 6 is perfect,
; as its proper divisors are 1, 2 and 3. The next perfect number is 28, given as 1 + 2 + 4 + 7 + 14. Write an iterative Scheme program
; perfect? which inputs an integer n > 1 and which outputs #t if n is perfect and #f otherwise. Use a local function, and try to make
; your program reasonably efficient.


; Problem 2b (15 points, including synergy) Give a complete proof of correctness for the function perfect? you developed in part a.


; Problem 3  (10 points) Write a Scheme function genper? which generalizes the function you developed for Problem 2a. Your function should have at
; least two parameters - n as before, a function parameter, combiner, and perhaps an additional parameter or so, if necessary. Show
; how to use genper? to test whether n > 1 is the product of all of its proper divisors. What are the pre- and post-conditions for
; your genper?


; Problem 4a (20 points)  Write a program reverse-number to reverse the digits of a number. For example, (reverse-number 0) = 0,
; (reverse-number 1234) = 4321. You may assume that the input is a non-negative integer.  You will likely need some auxilliary
; functions

; Problen 4b (15 points, including synergy) Give a proof of correctness for the function reverse-number you developed in part a.
; Concentrate on the proof of the main function, making use of the specifications you give for your auxilliary functions.




