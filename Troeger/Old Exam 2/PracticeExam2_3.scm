; CSc 335
; Spring 2020

; February 27

; First Exam - 1.25 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE:

; (I will email your graded paper to this address - DO NOT use a gmail address, as gmail seems to block name.scm 
;  files.  Please use your citymail address.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SAVE YOUR EXAM AS Lastname.Firstname.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1a - 30 points   -- 22
;;;; Problem 1b - 30 points   -- 28

;;;; Problem 2 - 40 points  -- 35


;;;; Total  85
;;;; Letter Grade  A


;;;; My comments in your exam are behind (at least) 4 semicolons (;;;;)
;;;;
;;;; Detailed scoring found below each problem



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework:

; no lists, no vectors, no strings, no assignment ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed in your backpack/briefcase.  They are not to leave the room, nor are
; they to be visible at any point during the exam.  Even the sight of a phone, or a flashdrive, or any other 
; electronic device not supplied by the course staff will result in our picking up your paper, resulting in an F
; grade for the exam.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  

; For both problems:  a complete development is one which features mutually reinforcing logic and code, 	
; resulting in a program which has been proved correct.  

; Grading will be based on your logic, your code, and the evidence of synergy between these. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 1a (30 points)  An integer p is a perfect square if if p = q^2, for some integer q.  Thus 4 is a perfect square,
; but neither 2 nor 3 is a perfect square.  It is known that an integer p >= 0 is a perfect square if and only if it has an
; an odd number of divisors.  In this problem you are asked to write a recursive procedure perfect-square? based on
; this fact.  Your procedure may not use square, nor sqrt.

; INSERT YOUR ANSWER HERE

;we need to develop an recursion method which is based on the fact that we have odd number of devisors
;We will need a helper function findDvisvisorNum: to find out how many devisor we have

; how we determine if a is a devisor is easy: (= 0 (modulo num a))
;we can iterave a from 1 to n , each if a is evently divided by num, then we return ( + 1 ( findDvisvisorNum num a+1)), which will continue to look for the remaining devisor number  +  1
;otherwise, we just continue to call ( findDvisvisorNum  a+1)

;For the stopping condition of perfect-square, only when a > num, as we know a is increasing by one each time, it must stop

;How we know if findDvisvisorNum is correct,we can prove it by very simple induction
;At the basic step, num is 0, we will return 0 based on the code

;;;; no -- your findDivisorNum stops not when num = 0, but when (> a num) ...



;IH:  we assume (findDvisvisorNum k) can return correct number of divisors, when k is an none negative integer

;IS: a = k-1, base on the code, it will return either (1+ (findDvisvisorNum k)) if a-1 is a dicisor or (findDvisvisorNum k) a-1 is not a divisor
;after we find out how many divisor we have, if it is odd number, (= 0 (modulo (findDvisvisorNum 1) 2)), ;this is a perfect square otherwise it is not


;;;; you have the induction backwards!  Notice that what shrinks here is the gap between a and num --  


(define (perfect-square num)
  (define (findDvisvisorNum a)
    (cond ((> a  num) 0)
          ((= 0 (modulo num a)) (+ 1 (findDvisvisorNum (+ 1 a))))
          (else (findDvisvisorNum (+ 1 a)))
    )
  )
  (cond ((= 0 (modulo (findDvisvisorNum 1) 2)) #f)
        (else #t)
   )
)
(perfect-square 24)
(perfect-square 25)


;;;; Problem 1a Section R Exam 1 Scoring
;;;;
;;;;
;;;; Design discussion, leading to guess code and guess induction hypothesis and termination argument (out of 10)  -- 5
;;;;
;;;; Final recursive and working code (out of 10) -- 10
;;;;
;;;; Final induction argument:  Basis, IH, IS (out of 7) -- 4
;;;;
;;;; Tests  (out of 3)  -- 3
;;;;
;;;; Total Problem 1a  -- 22
;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 1b (30 points) Now write an iterative procedure perfect-square-I? to solve the same problem.  Make use of 
; the fact that a number n is a perfect square if and only if it is a sum of consecutive odd numbers, starting at 1.  

; INSERT YOUR ANSWER HERE

;precondition: num is a none negative integer
;postcondition: return true is num is a perfect square, otherwise return false

;we need to based on the fact that a perfect square is the sum of a sequence of odd number start at 1.
;We can use a helper function calSumOdd to calculate the sum of odd sequence start at 1, we will need
;three variables, num is unchanged, a is a iterator start from 1 and increase by 2 each time(which means
;is the next unprocess odd number).sum is the sum of all odd number from 1 to a-2

;there are two termination condition for calSumOdd, the first one is (> sum num) which will return false, ;
;because sum will no longer equal to num. The other condition is (= sum num) #t) which will return true, ;because this is the fact we based on

;Beside these two termination condition, we assume the stop number is a = M. our GI is sum of all odd
;number < =M  =  sum of all processed odd number(result) +  sum of all unprocessed odd number (a to ;M)
;for each iteration, we will add a to the result, GI will become sum of all odd number  = result +a +  sum ;
;of all unprocessed odd number (a+2 to M)


;;;; just say 'sum is a sum of consecutive odd numbers, starting at 1'
;;;;
;;;; in other words, there is no need to mention your M


;Based on this GI, our iteration body will be (calSumOdd (+ 2 a) (+ a sum))


(define (perfect-square-I num)
   (define (calSumOdd a sum)
       (cond ((> sum num) #f)
             ((= sum num) #t)
             (else (calSumOdd (+ 2 a) (+ a sum)))
        )
   )
  (calSumOdd 1 0)
)
(perfect-square-I 25)
(perfect-square-I 7)


;;;; Problem 1b Section R Exam 1 Scoring
;;;;
;;;;
;;;; Design discussion, leading to guess code and guess invariant and termination argument (out of 10) -- 9
;;;;
;;;; Final iterative and working code (out of 7) -- 7
;;;;
;;;; Final certification:  weak enough, strong enough, invariance (out of 10)  --  9  (I can see these in your
;;;; design discussion, but not by name, and not cleanly separated out)
;;;; 
;;;;
;;;; Tests  (out of 3)  3
;;;;
;;;; Total Problem 1b  -- 28






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Problem 2 (40 points)  Give a complete development of an iterative procedure sum-perfect-squares which inputs
; two numbers m and n and which returns the sum of the perfect squares between m and n.  You may use either one of
; the perfect-square predicates from Problem 1, but you must make this function local -- that is, taking full
; advantage of variables' scope, you are to recast the perfect squares predicate as a function private to 
; to sum-perfect-squares.

; Make your precondition as weak as possible.



; INSERT YOUR ANSWER HERE


;Predition: input two none negative integer m and n, m is smaller or equal to n


;;;; there is no reason why m and n need to be non-negative, and no reason why they need to be integers!  Just
;;;; write a wrapper function to deal with these possibilities


;Poscondition:return the sum of all perfect number in between

;We will make use of the previous local function perfect-square-I, which can determine if it is a perfect number.

;We will need another helper function calSumPerfect For this function, the post condition is the same as sum-perfect-squares, 
;for the predcondition we will need result = 0 to store the sum of perfect ;number. And ' a'  as a interator which start as m

;GI:The sum of perfect number = the sum of process perfect number + the sum of unprocess perfect number
;At the beginning of the iteration,  The sum of perfect number = the sum of unprocess perfect number
;when this porgram terminate, sum of perfect number = the sum of process perfect number

; for the next iteration, first we will need to check if a is a perfect number, if yes ,then return 
; (calSumPerfect (+ 1 a) (+ a result))), else return (calSumPerfect (+ 1 a) result)). The GI will remain true ;
;for this iteration; The sum of perfect number = the sum of process perfect number + (a or 0) + the sum ;of ;unprocess perfect number( from a+1 to n)

;for termination , a will increase by one each time, so when a > n, we return the result



(define (perfect-square-I num)
   (define (calSumOdd a sum)
       (cond ((> sum num) #f)
             ((= sum num) #t)
             (else (calSumOdd (+ 2 a) (+ a sum)))
        )
   )
  (calSumOdd 1 0)
)

(define (sum-perfect-squares m n)
       (define (perfect-square-I num)
           (define (calSumOdd a sum)
              (cond ((> sum num) #f)
                ((= sum num) #t)
                (else (calSumOdd (+ 2 a) (+ a sum)))))
           (calSumOdd 1 0)
       )
       (define (calSumPerfect a result)
         (cond ((> a n) result)
               (else (cond ((perfect-square-I a) (calSumPerfect (+ 1 a) (+ a result)))
                           (else (calSumPerfect (+ 1 a) result))
        ))))
  (calSumPerfect m 0)
)
(sum-perfect-squares 1 10)
(sum-perfect-squares 1 5)




;;;; Problem 2 Section R Exam 1 Scoring
;;;;
;;;; Discussion leading to weakest possible precondition  (out of 5)  0
;;;;
;;;; Design discussion, leading to guess code and guess invariant and termination argument (out of 10) 
;;;;
;;;; Final iterative and working code with local perfect-square predicate (out of 10)
;;;;
;;;; Final certification:  weak enough, strong enough, invariance (out of 10)
;;;;
;;;; Tests  (out of 5)  
;;;;
;;;; Total Problem 2  -- 35  







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







