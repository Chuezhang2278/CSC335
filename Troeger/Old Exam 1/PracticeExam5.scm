; CSc 335
; Spring 2019

; March 7

; First Midterm Exam - 1.75 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 
; (I will email your graded paper to this address - DO NOT use a gmail address, as gmail seems to block name.scm 
;  files.  Please use your citymail address.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - final code (out of 10 points)   7
;;;; Problem 1 - development (out of 15 points)   8

;;;; Problem 2 - code (out of 15 points)  8

;;;; Problem 3 - final code (out of 20 points)  5
;;;; Problem 3 - development (out of 40 points)  15



;;;; Total  43
;;;; Letter Grade  C

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework:

; no lists, no vectors, no strings, no assignment...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed in your backpack/briefcase.  They are not to leave the room, nor are
; they to be visible at any point during the exam.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  

; Problem 1 (25 points) Using an invariant, give a complete development for an iterative procedure aux
; to simulate a 3-digit one-button counter which rolls over from 999 to 000. Your procedure should
; have parameters hundreds, tens, and ones, perhaps among others.  The procedure aux is to be
; called by another function, count: the call (count start k) returns the 3-digit number formed
; from the digits hundreds, tens and ones which would result when the step button on the simulated counter
; is pressed k times with initial value start.  

; Thus (count 0 1) returns 1, (count 95 10) returns 105, and (count 995 10) should return 5.  You
; should give a specification for the function count as well as for your iterative function aux.  

; Of course I do not want you to use modulo -- again, we are thinking of aux as an old-fashioned single-button counter:
; press the button once and 000 goes to 001, press it again and the display goes to 002.  The parameters
; hundreds, tens and ones change as they would on the single-button counter.  

; The only difference is that our simulator cannot output leading zeros -- we are at present constrained to
; use scheme numbers.

; As in class, a complete development is one which shows all of the guess-invariants and guess-designs and guess-programs,
; along with the reasons (arguments) for changing these as you make progress, and which culminates in a working program.
; I want to see a record of your development process.  I also need to see working tests for each of your functions.  

; 999 rolls back to 000

;gD1: {
; hundreds <- hundreds +1 
; tens <- tens + 1
; ones <- ones + 1
; hundreds * 100 + tens * 10 + ones = current count
; hundreds * 100 + tens * 10 + ones = start + k => stopping condition
; initialize hundreds, tens, ones as 0
;}


; gI1:  hundreds * 100 + tens * 10 + ones = current count
;(define (count start k)
;  (define (aux hundreds, tens, ones)
;      (define sum-product (+ (* hundreds 100) (* tens 10) ones))
;    (cond ((= sum-product (+ start k))))))
; wrong dD idea because it did not consider the feature of rolls back
; need one more condition to check the final sum-product with the feature of rolls back i.e. test 2 failed

; test 1: does initialization makes gI1 true?
; 0 * 100 + 0 * 10 + 0 = 0 where 0 is current count TRUE


;(define (aux hundreds tens ones)

(define (count start k)
  (define (aux hundreds tens ones)
      (define sum-product (+ (* hundreds 100) (* tens 10) ones))
    (cond ((= sum-product (+ start k)) sum-product)
          ((= sum-product (- k (- 1000 start) )) sum-product)
          ((= sum-product 999) (aux 0 0 0) (= start 0))
          ((< ones 9) (aux 0 0 (+ ones 1)))
          ((= ones 9) (aux 0 (+ tens 1) 0))
          ((and (= tens 9) (= ones 9)) (aux (+ hundreds 1) 0 0))   
          )
    )
  (aux 0 0 0))

;;;; nice idea for completely avoiding modulo, yet still having the wrapper function!

;;;; BUT:  you'll see that (count 995 20) does not return anything ... neither does (count 995 50)

;;;; More -- the transition from 009 to 010 should be precisely that -- ones should be set to 0, and tens to 1 (similiarly for
;;;; the transition from 099 to 100, etc.

;;;; 



; test 2: does it stop well?
; when the sum-product = start + k or (- k (- 1000 start) ) , it returns the final count 

; test3: is gI1 perserved from current call to next call? yes, hundreds, tens and ones are incremented by each condition
; which increment the overall current count. 

;(count 0 1) test case passed
;(count 995 10) test case passed
;(count 995 10)
;(count 995 20)

          
              


              
; Problem 2 (15 points)  We are all familiar with mathematicians' use of functions defined
; by cases.  For example, the absolute value function (abs x) is defined to be x if x is
; greater than or equal to 0, and -x otherwise.  In this problem you are to build a function
; by-cases which inputs two unary functions f and g, as well as a unary predicate pred -- you
; may assume that f and g and pred all expect a number as input -- and which returns the function
; (by-cases f g pred) such that

;              ((by-cases (lambda (x) x) (lambda (x) (- x)) (lambda (x) (>= x 0))) -4)

; returns 4, as does

;              ((by-cases (lambda (x) x) (lambda (x) (- x)) (lambda (x) (>= x 0))) 4)

; No development or proof is expected for this problem, but I do need to see a working test.



(define (by-cases f g pred)
  (lambda (number)
    (cond ((pred number) (f number))
          (else (g number)))))
;((by-cases (lambda (x) x) (lambda (x) (- x)) (lambda (x) (>= x 0))) -4) 
;((by-cases (lambda (x) x) (lambda (x) (- x)) (lambda (x) (>= x 0))) 4) 


;;;; neither f nor g are ever called!






; Problem 3 (60 points)  Without using iteration, give a complete development for a
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

; every function is recursive

; pre: n and m are positive integers and m <= n 
; post: returns the largest number which can be formed from any m digits in n

; Basis: when m = 1, largest-number-formed return the largest single digit of n
;; the call follows the 1st cond and returns the largest single digit

;(define (largest-number-formed n m)
;  figure out how keep track of largest digit
;  (define rest (quotient n 10))
;  (define last-digit (remainder n 10))
;  (cond ((= m 1) largest-single-digit)
;        (else (largest-number-formed rest (+ (* largest-digit m) (* other-digit (-  m 1))

;(define (largest-number-formed n m)
;  (define rest (quotient n 10))
;  (define last-digit (remainder n 10))
;   pre: n is a positive integer
;;   post: returns the largest digits of n
;  (define (largest-digit n) (cond ((= (remainder n 10) n) n)
;                              ((> last-digit (remainder rest 10) last-digit)) 
;                              (else (largest-digit rest))))
;  (cond ((= m 1) largest-digit)
;        (else (largest-number-formed rest (+ (* (largest-digit n) m) (* (largest-digit other-digit)(- m 1))



(define (largest-number-formed n m)
  (define rest (quotient n 10))
  (define last-digit (remainder n 10))
  (define pre-digit (remainder (quotient n 10) 10))
  
  (cond ((= m 1) (largest-digit n))
        ))

;IH:  largest-number-formed return the largest number formed by k digits

;IS: largest-number-formed return the largest number formed by k+1 digits

;;;; what has happened to m in this IH and IS?

;;;; the code is not yet in condition to clearly indicate your design / proofs are not given / tests are not given --

;;;; it is not clear to me that what you write here will deliver the correct m-digit number.  You seem not to take powers of
;;;; 10 into account.



; prove for largest-digit
; basis: if n is a single digit number, itself is the largest digit

; IH: largest-digit returns the largest-digit among other  k digits of n

; IS: largest-digit returns the largest-digit among other  k+1 digits of n                                
                               

                          
  
  




