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

;;;; Problem 1 - 50 points    31

;;;; Problem 2 - 50 points    35


;;;; Total   66
;;;; Letter Grade  B



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


; Problem 1 (50 points)  Give a complete development of a properly recursive procedure sum-odds which inputs
; two numbers m and n and which returns the sum of the odd integers between m and n. 

; Start by finishing the specification - don't put any unnecessary restrictions on the inputs: for example, 
; don't assume they are positive, or even integers, unless absolutely necessary.

; INSERT YOUR ANSWER HERE

;pre-condition: m and n are integers (negative or non-negative)

;;;; no attempt to deal with floats?


;post-condition: (sum-odds m n) = Sum of the odd integers between m and n

;;;; what, exactly, do you mean by 'between'?


;basis step: Our basis step is if n = m. In this senario,
;if n is odd, it will be added to the total sum, if m is odd, it will be added to the total sum. If none of them are odd,
;we will add nothing to the total sum
;inductive-assumption: We assume (sum-odds m n) works. If m is odd: (sum-odds (+ m 1) n) = Sum of the odd integers between m and n so far

;;; what does m odd have to do with the IH?


;inductive step: for the kth call, if m is odd: (+ m (sum-odds (+ m 1) n)) = (sum-odds (+ m 1) n) + m  which is clearly equal


;;;; what if m is not odd?


;for the kth call, if n is odd: (+ n (sum-odds m(+ n 1))) = (sum-odds m(+ n 1))) + n which is clearly equal


;;;; easier to swap m and n at the front end if n < m


;termination argument: We know the program terminates becuase either we are either increasing n by 1, or m by 1 exclusively at every step. After doing this
;for multiple iterations, eventually m = n which leads to termination.

(define (is-odd? num)
  (if (= (remainder (round num) 2) 0) #f
      #t))

;;;; or just use the primitive odd?


(define (sum-odds m n)
  (cond ((equal? m n) (cond ((is-odd? m) m) ((is-odd? n) n) (else 0)))
        ((<= m n)
         (if (is-odd? m) (+ m (sum-odds (+ m 1) n))
             (sum-odds (+ m 1) n)))
        ((>= m n)
         (if (is-odd? n) (+ n (sum-odds m (+ 1 n)))
             (sum-odds m (+ 1 n))))))

(sum-odds 0 3)


;;;; Problem 1 Section M Exam 1 Scoring
;;;;
;;;;
;;;; Precondition discussion leading to weakest precondition (out of 10)  3
;;;;
;;;; Design discussion, leading to guess code and guess induction hypothesis and termination argument (out of 10)  8
;;;;
;;;; Final recursive and working code (out of 10)  8
;;;;
;;;; Final induction argument:  Basis, IH, IS (out of 15)  12
;;;;
;;;; Tests  (out of 5)  none given, so 0 (ouch)
;;;;
;;;; Total Problem 1   31
;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Problem 2 (50 points)  Now give a complete development of an iterative solution to the same problem.  Your solution should consist of 
; a single top-level function (call it sum-odds-I) which makes optimal use of a single internal auxiliary function  
; (call it sum-iter).  

; INSERT YOUR ANSWER HERE


(define (sum-iter m n result)
  (cond ((equal? m n) (cond ((is-odd? m) (+ m result)) ((is-odd? n) (+ n result)) (else result)))
        ((and (<= m n) (is-odd? m)) (sum-iter (+ m 1) n (+ m result)))
        ((and (>= m n) (is-odd? n)) (sum-iter m (+ n 1) (+ n result)))
        ((<= m n) (sum-iter (+ m 1) n result))
        (else (sum-iter m (+ n 1) result))))

(define (sum-odds-I m n)
  (sum-iter m n 0))

;pre-condition: m and n are integers (negative or non-negative)
;post-condition: (sum-odds-I m n) = Sum of the odd integers between m and n
;guess-invarient: if m <= n and m is odd: sum of the odd integers between m and n(processed) = m + sum of the odd integers between m and n(unprocessed)
;                 if m >= n and n is odd: sum of the odd integers between m and n(processed) = n + sum of the odd integers between m and n(unprocessed)


;;;; not quite: you need to say something about the initial value (M) of m -- eg, the sum of the odds between M and N = the sum of the odds between
;;;; M and m +  the sum of the odds in unprocessed

;;;; but you are definitely on the right track





;initialization: During initlization, we can see that our result is set to 0.
;if m <= n and m is odd
; sum of the odd integers between m and n(processed) = m + 0 = m which is true
;if m >= n and n is odd
; sum of the odd integers between m and n(processed) = n + 0 = n which is true
;maintenence: In the middle,sum of the odd integers between m and n(so far) will contain all the sum of odds from 0..n
;termination: We know the program will come to an end when m = n. This will eventually happen since in each iteration,
;we are increasing either m or n.


;;;; Problem 2 Section M Exam 1 Scoring
;;;;
;;;;
;;;; Precondition discussion leading to weakest precondition (out of 5)  -- had you given a wrapper to deal with floats, there would have been
;;;;           the need to talk about whether it achieves your invariant:  0
;;;;
;;;; Design discussion, leading to guess code and guess invariant and termination argument (out of 15)  12
;;;;
;;;; Final iterative and working code (out of 10)  10
;;;;
;;;; Final certification:  weak enough, strong enough, invariance (out of 15)  13
;;;; Tests  (out of 5)  -- again, you are losing points for a silly reason: as I said numerous times in class, I want to see tests!  0
;;;;
;;;; Total Problem 2  35





