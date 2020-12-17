; CSc 335 Section R
; Fall 2020

; December 15

; First Part of Final Exam - 1 hour

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SAVE YOUR EXAM AS Lastname.Firstname.PART1.scm

; QUESTIONS DURING THE EXAM: email me at dtroeger@ccny.cuny.edu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; INSTRUCTIONS (TWO parts, TWO mailings)

;; AFTER YOU RECEIVE EACH PART OF THE EXAM

; start solving the exam questions in DrRacket as soon as you receive them

; FOR PART 1: NO LATER THAN 1630, stop writing, SAVE YOUR SOLUTION, AND EMAIL it to me at dtroeger@ccny.cuny.edu.  Your exam MUST be named
; with your name: Lastname.Firstname.PART1.scm, and it MUST include everything I have written here as well as your solution.

; Your email MUST be timestamped no later than 1635.

; I will email you PART 2 upon receiving your PART 1.  If you finish PART 1 early, then send it to me when you are done -- I will send
; PART 2 out to you right away.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Scoring Template - Do NOT Erase!


;;;; Part 1:
;;;; Part 2:  (next mailing)

;;;; Total:

;;;; Letter Grade


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework:

; no vectors, no strings, no assignment ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: I trust you to work alone on this exam, with your only reference
; the R5RS language manual.

; Any instance of cheating will be dealt with according to CCNY/CUNY rules: an F in the course, since this is the
; final exam.  This policy was announced in the course syllabus.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR RESPONSE INTO THE EXAM DIRECTLY FOLLOWING THE QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recall:  a complete development is one which features mutually reinforcing logic and code,    
; resulting in a program which can be proved correct.

; Grading will be based on your displayed strategy, your logic, your code, and the evidence of synergy between these.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Part 1 of the Exam  -- 15 points

; Give a complete development and proof of a purely functional R5RS Scheme program which inputs a positive integer
; n and which returns two positve integer factors p and q of n such that (i) pq = n, and (ii) among all pairs (a,b) of positive integer factors of n
; with ab = n, p and q are closest.  For example, if n = 200, some such factor pairs are (1,200), (2, 100), and (4,50).  But the pair with
; closest factors is (10,20).  If n is prime, your program should return just (1,n).

; Helper functions which were not developed in class must be fully developed and proved here. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; THINK BEFORE YOU START CODING!!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;=========================================================================================== 
; Development

; For this question i will be trying to split it up into multiple parts.
; i. I will create a function to get all pairs of factors
; ii. then i will find the min difference between all of the pairs
;===========================================================================================
; The way this function works is that it will find all factors of a number as shown below (modulo integer divisor) = 0
; If it is 0 then i can go ahead and find what its pair is by doing (/ integer divisor) and just putting them in a list
(define (factors integer)
  (define (main integer divisor)
    (cond ((zero? integer) 0)
          ((= integer divisor) '())
          ((= (modulo integer divisor) 0) (cons (list divisor (/ integer divisor)) (main integer (+ divisor 1))))
          (else (main integer (+ divisor 1)))))
  (main integer 1))

; Testing Function 
;(factors 100)
;(factors 1000)
;(factors 64)
;(factors 17)

;Proof
;GI = Total_work = Work_done + Work_remaining
;GI = List_of_pairs_of_factors = (pairs_of_factors_so_far) + (factors_unprocessed)
;Pre-Cond: Positive Integer, Divisor < Integer 
;Post-Cond: List of pairs of Factors
;
;Weak-Enough? (GI + Pre): Assuming the pre-condition is true, then the divisor will always
;                         be less than the integer upon first call of the function such that
;                         the GI would be '() + (factors_unprocessed)
;
;Strong-Enough? (GI + Post): Assuming the post-condition is true then for the last call of the
;                            function, what will be returned is a list of integers such taht 
;                            the GI would then be '(list of pairs of factors) + '())
;                            which shows that GI is true for the last call.
;
;Preserved?: Since we are assuming that the pre-condition is true, then as long as function call
;            k exists, then function call k+1 exists. Call k+1 either satisifies the post condition
;            and will terminate or it will satisfiy the pre condition and continue onto call k+2
;
;Termination?: Like said above, Assuming the pre-cdon and post-cond is true, then termination is
;              eventual such that there will be a point in the function call where (divisor = integer)
;              and the program will terminate and the recursive call will break.
;=========================================================================================== 
;so that i wouldn't be as messy, i created this function to find the difference of the pairs
 
(define (func pairs)
  (cond ((null? pairs) 0) ;<- this is just for testing purposes and JUST in case
        (else (abs (- (car pairs) (car (cdr pairs)))))))
;===========================================================================================
; Simply a modified version of looking for the smallest digit in a list. Just with pairs instead
; No logic difference
(define (findMin pairs)
  (define (main pairs largest)
    (cond ((null? pairs) largest)
          ((null? (cdr pairs)) largest)
          ((> (func largest) (func (car pairs))) (main (cdr pairs) (car pairs)))
          (else (main (cdr pairs) largest))))
  (main (cdr pairs) (car pairs)))

;Proof
;GI = Closest_pair = (pairs_evaluated) + (Pairs_not_yet_evaluated)
;Pre-condition: Non-empty list of pairs
;post-condition; closest pair

;Weak-enough? (GI + Pre): Assuming the Pre-condition is true then on the first call
;                         of the function, we will have (pairs_evaluated(k)) + (pairs_not_yet_evaluated(n-k))
;                         where k is 0 and n is the # of pairs left to be evaluated. With this in mind
;                         we can notice that the GI is represented as '() + (pairs_not_yet_evaluated)
;
;Strong-Enough? (GI + Post): Assuming the Post-Condition is true, then on the last call of the function
;                            we will have (pairs_evaluated(k+n)) + (Pairs_not_yet_evaluated(0)
;                            where all the pairs would have been evaluated by this point returning
;                            the closest pair with a GI that is represented as (pairs_evaluated) + '()
;
;preserved? Assumign the Pre-condition is true then for some call k, there exists a call k + 1
;           such that the size of the list is greater than 1. If it is not greater than one then
;           the program terminates accordingly.
;
;Termination? Assuming Pre-condition is true, we can see that what is returned per func call
;             is the smallest pair with a list of pairs to be processed and the list of pairs to
;             be processed will eventually be empty so it will eventually terminate
;===========================================================================================
;Im compiling all of the functions here
(define (Main integer)
  (cond ((negative? integer) #f)
        ((zero? integer) 0)
        ((= integer 1) 1)
        (else (findMin (factors integer)))))

        

          
    







