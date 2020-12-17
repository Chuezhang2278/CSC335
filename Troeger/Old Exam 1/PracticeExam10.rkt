; CSc 335 Section R
; Fall 2020

; October 13

; First Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (I will email your graded paper to this address - DO NOT use a gmail address, as gmail seems to block name.scm
;  files.  Please use your citymail address.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SAVE YOUR EXAM AS Lastname.Firstname.scm

; QUESTIONS DURING THE EXAM email me at dtroeger@ccny.cuny.edu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; INSTRUCTIONS

;; AFTER YOU RECEIVE THE EXAM

;; start solving the exam question in DrRacket as soon as you receive it
;
;; AT PRECISELY 4:45, stop writing and SAVE YOUR EXAM
;
;; BEFORE 4:50 -- email your Lastname.Firstname.scm exam solution file to me at dtroeger@ccny.cuny.edu


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; LATE ARRIVING EXAMS WILL BE PENALIZED 2 POINTS FOR EVERY MINUTE LATE, STARTING AT 4:52. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Scoring Template - Do NOT Erase

;;;; Score (15 points possible)  2, for 13%

;;;; Letter Grade  F


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this EXAM. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the quizzes or homework:

; no lists, no vectors, no strings, no assignment ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: I trust you to work alone, with your ONLY reference
; the R5RS language manual.  SEARCHING FOR ANSWERS ON THE WEB IS CHEATING.  WORKING WITH ANYONE
; ELSE IS CHEATING.  LOOKING UP ANSWERS IN A BOOK IS CHEATING. DO NOT CHEAT!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR SOLUTIONS INTO THIS DOCUMENT, IMMEDIATELY FOLLOWING THE PROBLEM STATEMENTS.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Here is the exam problem.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; WITHOUT USING ITERATION, give a complete development (specification, design, code and proof) for a
; procedure which inputs two positive integers n and m, and which returns the largest number which can
; be formed from any m digits in n.

; For example, if n = 11111 and m = 3, then your (main) program will return 111.  If n = 14253 and m = 2,
; your program will return 54.  Of course m cannot exceed the number of digits in n.

; For this problem, whose solution almost certainly requires auxilliary functions, I want to see
; justification for the functional decomposition which leads to the auxilliary functions.  I want to see
; a specification and an induction argument for each recursive function, and I want your certification
; arguments to make full use of pre- and post-conditions for the auxilliary functions whenever these are
; called.  Give clear definitions of any terms you introduce to describe your functions. 

; You are perfectly free to use ("built-in") scheme primitives such as quotient and expt -- there is
; no need to build and prove an exponentiation function, for example.

; As always, I need to see working tests of your functions. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SPECIFICATIONS
;Pre-condition: m >= 0, n > 10 and m < # of digits n
;Post-condition: Largest digit and m

; DEVELOPMENT
;We are given an input n and a length m so the first thing that comes into my mind is we would go through the digits
;And find the largest digit in the input and return it by addition

;1. The largest combination of numbers should basically be a DecreasingOrder function with certain specifications

;2. We should also have a base case such that if the # of digits present is less than 2, then just return the value

;3. We need a stopping condition such that if input n < 10^(m-1)
;   The justification in this is that if m = 3 and 10^(3-1) = 100, this means that n will terminate when the digit count is less than 3 

;4. ; (define (FindLarge m) = largest value
;     This should be an auxilary function to find the largest digit

;5. We will also need an auxilary function that can return the digit placement of the largest value because we will need a method of removing the digit that is already processed
;   Look below for instance, we found a 5 so we replace it with a 0. We find 4, replace it with a 0. and continue until m = 0.

;Idea: n = 15342, m = 3
;    : 15342 | 0 
;    : 10342 | 5
;    : 10302 | 54
;    : 10002 | 543
;    : Stop

; We don't need to worry if it is the biggest number possible or not because digits will be returned in decreasing order of largest to smallest

; I will be inducting on the total digits remaining

; I previously stated that i would terminate when the digit count is less than 3 but i can just make it so that it terminates when m = 0.
; I can run the program recursively such that it decrements m by 1 every call and when m = 0, terminate.

; Another thought that i did not implement is that largest digit is found iteratively, perhaps a method of finding it recursively would be to have it check through
; Two numbers at a time to see if it is the largest or not and if it isn't replace

; I tried to make a method that is recursive right under the iterative but it turns out it is still iterative and they are actually the same thing.

; Going back to my statement about decreasingOrder, this essentially can be seen as a sorting question but i am unsure of how to sort recursively

;;;;  the problem can be solved without sorting!  

;CODE

; This Finds the Largest Digit (ITERATIVE)
(define (FindLargest n Large)
  (if (< n 10) (if (> Large n) Large n)
      (if (> (remainder n 10) Large) (FindLargest (quotient n 10) (remainder n 10))
          (FindLargest (quotient n 10) Large))))

;ATTEMPT AT RECURSIVE
(define (Largest x large)
  (cond ((< x 10) large)
        ((> (remainder x 10) large) (largest (quotient x 10) (remainder x 10)))
        (else (Largest(quotient x 10) large))))

;;;; this is still iterative.  You may want to try using max as the deferred operator




; This finds the placement of the Largest Digit Of course this is used under the assumption that the digit we are looking for is present
(define (FindDigit n Large Count)
  (if (= Large (remainder n 10)) Count
      (FindDigit (quotient n 10) Large (+ Count 1))))

;Dummy psuedo code
;As explained above, everytime a largest integer is processed, subtract it from n such that a 0 is remaining
(define (Main n m count)
  (if (> m 0) (+ (* (Largest n 0)) (expt 10 count))) (Main (- n (* (Largest n 0)) (expt 10 (FindDigit n (Largest n 0) 0))) (- m 1) (+ count 1))
      0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;^  here is the end of the if

;;;; you are here sequencing two expressions!


(Largest 11423 0)
(FindDigit 11423 4 0)
(Main 11423 3 0)

;PROOF
;Finding Largest Digit
;Basis: n > 10 [the number of digits is greater than 1]
;IH: We assume that this call works such that the largest of two integers is returned
;IS: If the current call works then that means post condition returns either the largest digit in the integer or will continue looking for the nth largest digit
;  : until it terminates.

;Finding the Digit Placement
;Basis: LargestDigit is a digit of n
;IH: This call will work if and only if the digit is present in n
;IS: If not found for the current digit, the call will continue to the n+1th digit eventually finding the digit under the assumpion that Largest digit is a digit of n

;Main Function
;Basis:  m >= 0, n > 10 and m < # of digits n
;IH: This call is assumed to work if m > 0 and that m < # of digits in n
;IS: It will work for any nth call such that m > 0. from 1 - mth call, it will continue calling assuming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rubric

;; Functional Decomposition (eg, removeRightmost, maxDigit, with both used by largestMDigit) [out of 3] 1
;; (if the first step is to try to sort the number, then a functional decomposition must be given for the sorting process)  1

;; Recursive Development of one helper function, including its specification and termination [out of 2]  0
;; Recursive Development of the main function, including its specification and termination [out of 3]  0
;; Coding the helper function graded for development and the main function [out of 3]  0

;; Tests - for each helper function, as well as for the main function [1]  0

;; Inductive proof of main function, including discussion of its dependence on the helper
;; functions  [out of 3]  0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Oral Exam Score  1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Combined Score for Exam 1      2  + 1, for 15%, for an F

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


