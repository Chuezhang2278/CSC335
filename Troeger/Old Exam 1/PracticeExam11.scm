; CSc 335 Section M
; Fall 2020

; October 13

; First Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:
; TYPE YOUR NAME HERE!!!!!!!

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; TYPE YOUR FULL EMAIL ADDRESS HERE!!!!!!!!!!


; (I will email your graded paper to this address - DO NOT use a gmail address, as gmail seems to block name.scm
;  files.  Please use your citymail address.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SAVE YOUR EXAM AS Lastname.Firstname.scm

; QUESTIONS DURING THE EXAM email me at dtroeger@ccny.cuny.edu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; INSTRUCTIONS

;; AFTER YOU RECEIVE THE EXAM

; start solving the quiz question in DrRacket as soon as you receive it

; AT PRECISELY 12:15, stop writing and SAVE YOUR EXAM

; BEFORE 12:20 -- email your Lastname.Firstname.scm exam solution file to me at dtroeger@ccny.cuny.edu


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; LATE ARRIVING EXAMS WILL BE PENALIZED 2 POINTS FOR EVERY MINUTE LATE, STARTING AT 12:22. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Scoring Template - Do NOT Erase

;;;; Score (15 points possible)

;;;; Letter Grade


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this EXAM. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the quizzes or homework:

; no vectors, no strings, no assignment ...

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


; A positive integer n > 1 is said to be +-perfect if it is the sum of its proper divisors. Thus 6 is +-perfect,
; as its proper divisors are 1, 2 and 3. The next +-perfect number is 28, given as 1 + 2 + 4 + 7 + 14.

; Similarly, a positive integer n > 1 is said to be *-perfect if it is the product of its proper divisors.  Thus 6 and
; 8 are *-perfect.

; We want a single program, op-perfect, which can be used to test whether a number is +-perfect or *-perfect, depending on
; whether op = + or op = *, respectively.

; Specify, design, develop, code and prove correct an ITERATIVE Scheme program op-perfect? which inputs an integer n > 1, an
; operator op, and which outputs #t if n is op-perfect and #f otherwise. USE A LOCAL FUNCTION, and try to make your program
; reasonably efficient. 
;1 * 2 * 4 * 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-divisor num count total operator)
  (if (> num count)
      (if (= (remainder num count) 0) (add-divisor num (+ count 1) (operator total count) operator)
          (add-divisor num (+ count 1) total operator))
      total))

(add-divisor 28 1 0 +)
(add-divisor 496 1 0 +)
(add-divisor 497 1 0 +)

(define (op-perfect num operator)
  (operator num num))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;