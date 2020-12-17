#lang sicp
; CSc 335 Section M
; Spring 2020

; April 30

; Second Exam - 1.25 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 

; (I will email your graded paper to this address - DO NOT use a gmail address, as gmail seems to block name.scm
;  files.  Please use your citymail address.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SAVE YOUR EXAM AS Lastname.Firstname.scm

; QUESTIONS DURING THE EXAM: email me at dtroeger@cs.ccny.cuny.edu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; INSTRUCTIONS (DISTRIBUTED VIA PIAZZA THE NIGHT BEFORE THE EXAM)

;; BEFORE I EMAIL YOU THE EXAM

; You are to host a Zoom meeting, with yourself as the only person attending

; Open DrRacket

; Share your DrRacket window (or your entire desktop) with the meeting

; Set Zoom to record the window


;; AFTER YOU RECEIVE THE EXAM

; start solving the exam questions in DrRacket as soon as you receive the exam

; AT PRECISELY 1215, stop writing, SAVE YOUR EXAM, and stop recording -- save the recording as an mp4 file (this will take quite a while, depending
; on your hardware)

; BEFORE 1220 -- email your Lastname.Firstname.scm exam solution file to me at dtroeger@cs.ccny.cuny.edu

; BEFORE 1315 -- once Zoom has finished saving/converting your recording, upload it to Dropbox.  Set a password on the uploaded recording.
; and then email me the link (right click on the file to share it) and password for your uploaded recording.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HELPFUL SUGGESTION FROM HELIX:  If you go in your settings and to recordings there's an option that says "Keep temporary recorded files",
; it says this will help recover zoom if there's an issue!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - 100 points


;;;; Total   16
;;;; Letter Grade  F

;;;; see my embedded comments

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework:

; no vectors, no strings, no assignment ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: I trust you to work alone on this exam, with your only reference
; the R5RS language manual

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR RESPONSE INTO THE EXAM DIRECTLY FOLLOWING THE QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here is the examination problem. 

; Recall:  a complete development is one which features mutually reinforcing logic and code,   
; resulting in a program which has been proved correct. 

; Grading will be based on your logic, your code, and the evidence of synergy between these.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Problem 1 (100 points)  Give a complete specification, development, working code and proof of a program which
; inputs a list of pairs of integers, and which outputs a list of pairs of the form (number (list of numbers))
; in which none of the output pairs have the same first element, and no list of numbers contains any duplicates,
; as follows.

; For input list '((1 3) (3 4) (1 3) (2 5) (1 6) (1 2) (3 7)) the output is to be
; '( (1 (3 2 6))  (3 (4 7))  (2 (5)) )



;; SOME DETAILS

; You see here that the input list contains (1 3) (two times), (1 6) and (1 2), and that these are all the
; input pairs with first element 1 -- and the output contains the pair (1 (3 2 6)), and no other pair with first
; element 1.  You see that 3 occurs only once in the right hand side of the output pair. You see that each right hand side of
; an input pair with first element 1 occurs precisely once in the right hand side of the output pair with first element 1.

; You see that the input list contains (3 4) and (3 7), and that these are all the input pairs with first element
; 3 -- and the output contains the pair (3 (4 7)), and no other pair with first element 3.  You see that each right hand side of
; an input pair with first element 3 occurs precisely once in the right hand side of the output pair with first element 3.

; You see that the input list contains just one pair with first element 2, namely (2 5), and the output list
; contains (2 (5)), and no other pair with first element 2.  You see that each right hand side of
; an input pair with first element 2 occurs precisely once in the right hand side of the output pair with first element 2.

; The order of the output pairs does not matter.

; The order of the elements in the right hand side of each output pair does not matter.



;; RECURSIVE? ITERATIVE?  AUXILLIARY FUNCTIONS?

; YOU ARE VERY LIKELY TO WANT TWO OR THREE AUXILLIARY FUNCTIONS -- you should give specs and proof sketches for any of these which
; involve looping or recursion. Be sure to give specs for any auxilliary functions.

; YOU ARE FREE TO USE EITHER RECURSION OR ITERATION IN ANY OF YOUR FUNCTIONS, SO LONG AS YOU INCLUDE ONE COMPLETELY
; PROVED RECURSIVE FUNCTION AND ONE COMPLETELY PROVED ITERATIVE FUNCTION.   


; INSERT YOUR ANSWER HERE

;pre-cond: input is list of pairs integers
;post-cond: output is in form of (num(list of numbers)) where no list of numbers contains duplicates, and num is unique.

;Development
;basis: if the input is null then return null
;Idea: Loop through the list of pair of numbers, for each pair, first check if the pair in res is already generated,
;      if no then generate a pair and append into res. For example: list '((1 3) (3 4) (1 3) (2 5) (1 6) (1 2) (3 7)), (1 3) is not in res, then append (1 (3)) into res.
;      Now go to the next element which is (3 4) by using cdr(car list), since it is also not in res, then append. Now res is ((1 (3)) (3 (4))).
;      Since the third element (1 3), 1 is already in res, then 3 is already in (1 (3)), so skipped
;      (2 5) not in res so append
;      For (1 6), we found 1 is in res, but 6 is not in (num (list of numbers)), so we append into (1 (3 6)). Now res is ((1 (3 6)) (3 (4)))
;      Same procedure for all remaining element in the input list
;      When reach to the end of the list '(), return res



;;;; this is exactly right -- your next task is to summarize your approach into a GI, and then code, with the idea of making progress towards termination
;;;; while maintaining the GI


(define (checkDup res target)

  (cond ((null? res) #f)
        ((= (caar res) (car target)) #t)
                                     
        (else (checkDup (cdr res) target))
  )
)

 (checkDup '( (1 (3 2 6))  (3 (4 7))  (2 (5)) ) '(2 3))

;;;; this returns #t -- not at all clear to me what (precisely) it is supposed to do.  How about giving specs?




; This is a helper function for checking if a current element is already in the result pair

;;;; result pair?  current element?  


; basis: if the res pair is empty, then return True because the current pair is not in it.
; IH: This function will return #t if the current element is already in the res
;     #f if not in the res
; IS: It will loop through the list by using cdr


;;;; this is not an induction argument -- you need to review before the final!!


; Termination : when reach to the end of the list which is '(), return #f automatically since target is not found.



;;;; Your checkDup function is iterative, however, and yet you give a standard induction for it: in this course, iterations need invariants!



;(res '((1 3) (3 4) (1 3) (2 5) (1 6) (1 2) (3 7)))
;(car(cdr (car '( (1 (3 2 6))  (3 (4 7))  (2 (5)) ) )

;;;;; ??????


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Scoring Rubric


;;;; overall design      4 -- Design discussion, leading to functional decomposition  (out of 5)
;;;;                     2 -- Justification for choice of recursive or iterative approach to auxilliary functions (out of 5)
;;;;                     -- Specs knitted together (out of 5)

;;;; iterative function  5 -- Spec (out of 5)
;;;;                     5 -- Design process leading to guess code and guess invariant (out of 10)
;;;;                     -- Final iterative and working code (out of 5)
;;;;                     -- Final certification:  weak enough, strong enough, invariance (out of 10)
;;;;                     -- Use of Tests (out of 5)

;;;; recursive function  -- Spec (out of 5)
;;;;                     -- Design discussion, leading to guess code and guess induction hypothesis and termination argument (out of 10)
;;;;                     -- Final recursive and working code (out of 5)
;;;;                     -- Final induction argument:  Basis, IH, IS (out of 10)
;;;;                     -- Use of Tests (out of 5)

;;;; working code        -- No output pairs with the same first element, no skipped elements (out of 5) 
;;;;                     -- No output pairs with duplicates in the second element (out of 5)
;;;;                     -- Use of Tests (out of 5)


;;;; total    16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;














  