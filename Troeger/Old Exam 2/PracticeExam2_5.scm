; CSc 335
; Spring 2019

; April 11 2019

; Second Midterm Exam - 2.0 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 

; (I will email your graded paper to this address - DO NOT use a gmail address, as gmail seems to block messages
;  with program attachments.  Please use your citymail address.)

; PLEASE SAVE YOUR FILE AS Lastname.Firstname.scm, or Lastname.Firstname.rkt.

; MAKE SURE THE FILE IS A TEXT FILE, and not a binary file, so we can check for successful upload at the end of
; the exam   (binary files result when you use the DrRacket box comments instead of semicolens).  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1a (out of 10 points)  9
;;;; Problem 1b (out of 10 points)   7

;;;; Problem 2a - (out of 15 points)  9
;;;; Problem 2b - (out of 15 points)  11

;;;; Problem 3 - (out of 20 points)

;;;; Problem 4 - (out of 20 points)

;;;; Problem 5 - (out of 20 points)



;;;; Total - 36 (out of 110 points)
;;;; Letter Grade  D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework:

; no vectors, no strings, no assignment...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed in your backpack/briefcase.  They are not to leave the room, nor are
; they to be visible at any point during the exam.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; READ THE ENTIRE EXAM, INCLUDING THE NEXT SECTION, CAREFULLY BEFORE STARTING

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Without subscripts or mathematical typography, it is perhaps best to start with an example.  The polynomial x^3 + 2x^2 + 1 can be written
; as a Scheme list in many ways, but let us agree that we will write it as ((1 * (x ^ 3)) + (2 * (x ^ 2)) + (1 * (x ^ 0))).  This is a 5 element
; list, with first element (1 * (x ^ 3)) -- itself a 3 element list, second element +, and so on.  

; Following this convention, a kth degree polynomial with leading coefficient ak can be written as
;           ( (ak * (x ^ k)) + ... + (a1 * (x ^ 1)) + (a0 * (x ^ 0)) )

; Here, each of ak, ..., a1 and a0 are real numbers, and x is a variable. 


; A polynomial ((a0 * (x ^ 0))) of degree 0 is just a constant; if a0 = 0, it is called the zero polynomial.
 

; Polynomials may be added and multiplied, according to rules familiar to you since basic
; algebra.  Two add two polynomials, for example, one adds coefficients of like powers:

;                   ( (3 * (x ^ 2)) + (1 * (x ^ 1)) + (2 * (x ^ 0)) ) + ( (1 * (x ^ 3))  + ( 1 * (x ^ 2)) + (1 * (x ^ 0)) ) =
;                     ( ( 1 * (x ^ 3)) + (4 * (x ^ 2))  + (1 * (x ^ 1)) + (3 * (x ^ 0)) )

; and similarly for multiplication: the result of operating on two polynomials is required to be a polynomial.  

; The class P of polynomial expressions is the least class consisting of the polynomials in x, and closed under
; addition and multiplication.  That is, P ::= polynomial | (P + P) | (P * P), where polynomial is assumed to
; be a polynomial in the single variable x. Note that beyond the base case, a polynomial expression is usually not a polynomial.
;( (3 * (x ^ 2)) + (1 * (x ^ 1)) + (2 * (x ^ 0)) ) + ( (1 * (x ^ 3)) + (1 * (x ^ 2)) + (1 * (x ^ 0)) ), for example, is a
; polynomial expression, but not a polynomial.   One needs to carry out the addition, and collect terms, and then order the
; terms according to decreasing powers of x, to obtain a polynomial.  

; You are asked, in Problem 5, below, after some initial steps (Problems 1 through 4) to design, implement and certify a program
; p-eval for symbolically evaluating polynomial expressions in a single variable.

; Thus, (p-eval '((3 * (x ^ 2)) + (1 * (x ^ 1)) + (2 * (x ^ 0))) + ((1 * (x ^ 3)) + (1 * (x ^ 2)) + (4 * x^ 1)) + (1 * (x ^ 0))) )
; returns ((1 * (x ^ 3)) + (4 * (x ^ 2)) + (5 * (x ^ 1)) + (3 * (x ^ 0)))
; with powers in decreasing order, and at most one term of each power.  

; As a first step, you will need to set up a data structure suitable for representing such polynomials.  Here you are
; to represent a polynomial as a 3-element list (leading-coefficient degree rest-of-polynomial).  Thus
; ((3 * (x ^ 2)) + (1 * (x ^ 1)) + (2 * (x ^ 0))) would be represented (3 2 p1), where p1 = (1 1 p2) is the representation of ((1 * (x ^ 1)) + (2 * (x ^ 0)))
; and p2 = (2 0 '()) is the representation of ((2 * (x ^ 0))). Note that the degree of rest-of-polynomial is less than that of polynomial. 
; The zero polynomial is represented (0 0 '()).  Expanding the subterms, one has (3 2 (1 1 (2 0 '()))) as the representation
; of ( (3 * (x ^ 2)) + (1 * (x ^ 1)) + (2 * (x ^ 0)) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Here are the examination problems.  Note that there are 110 points possible -- 10 points counting for extra credit.

; Problem 1a (10 points)
; Design and implement a data structure, as described above, for representing single variable polynomials with integer coefficients.  



; Problem 1b (10 points)
; Design and implement a data structure for representing polynomial expressions, as defined above.  
(define (make-+ p1 p2)
  (append p1 '(+) p2))

;;;;; append will not give the correct result -- list is needed


(define (make-* p1 p2)
  (list p1 '* p2))

(define (make-^ pow)
  (list 'x '^ pow))


(define (coeff p)
  (car p))


;;;;; makes no sense for p-exps


(define (operator p)
  (car (cdr p)))




(define (pow p)
  (car (cdr (car (cdr (cdr p))))))

;;;;; makes no sense for p-exps


  
;classifers
(define (plus-poly? p)
  (eq? (operator p) '+))

(define (times-poly? p)
  (eq? (operator p) '*))

(define (exp-poly? p)
  (eq? (operator p) '^))

;;;; nothing is said about exp-poly in my problem statement!







; Problem 2a (15 points, with 10 points for development and proof)
; Design, implement and certify a procedure rep-to-poly which inputs a representation r of a polynomial p, as defined above, and which returns
; the polynomial p, as a list.  For example, (rep-to-poly '(3 2 (1 1 (2 0 '())))) will return ( (3 * (x ^ 2)) + ( 1 * (x ^ 1)) + (2 * (x ^ 0)) ).
; Working test cases are required.

; A key design goal for 2a and 2b is that the composition of poly-to-rep and rep-to-poly should yield the appropriate identity function.

;pre: r is a valid representation i.e. 3 element list of a polynomial p
;post: returns the p as a list
;(define (rep-to-poly r)
;  (cond ((= (degree r) 0)
;         (make-* (leading-coeff r) (make-^ (degree r))))
;        (else (make-+
;               (make-* (leading-coeff r) (make-^ (degree r)))
;               (rep-to-poly (rest-of-poly r))))))


;basis: rep-to-poly returns degree 0 polynomial 
; returns a signle term p without + 
;(define test '(3 2 (1 1 (2 0 '())))) ;did not pass need to fix the else clause
;(define test2 '(2 0 '())); pass
;(rep-to-poly test2)
;(rep-to-poly test)

;
;(define (rep-to-poly r)
;  (cond ((= (degree r) 0)
;         (make-* (leading-coeff r) (make-^ (degree r))))
;        (else (make-+
;               (make-* (leading-coeff r) (make-^ (degree r)))
;               (rep-to-poly (rest-of-poly r))))))


;;;;; You overlook the possibility that a polynomial might have just a single term, perhaps of non-zero degree



;IH: rep-to-poly returns degree n polynomial
;IS: rep-to-poly returns degree n+1 polynomial

;(define test '(3 2 (1 1 (2 0 '())))) ;did not pass need to fix the else clause
;(define test2 '(2 0 '())); pass
;(rep-to-poly test2)
;(rep-to-poly test)


;;;; missing quite a lot of the certification ... 


;terminate: works by cdring down the r because the length of r is integer that's decreasing



; Problem 2b (15 points, with 10 points for development and proof)
; Design, implement and certify a procedure poly-to-rep which inputs a polynomial p, as a list, and which returns the representation of p.  For example,
; (poly-to-rep '( (3 * (x ^ 2)) + ( 1 * (x ^ 1)) + (2 * (x ^ 0)) ) should return (3 2 (1 1 (2 0 '()))).
; Working test cases are required.
; A key design goal for 2a and 2b is that the composition of poly-to-rep and rep-to-poly should yield the appropriate identity function.



;(define test4 '( (3 * (x ^ 2)) + ( 1 * (x ^ 1)) + (2 * (x ^ 0)) ))
;(poly-to-rep test4)
              
; Problem 3 (20 points, with 15 points for the development process and proof)
; Design, implement and prove correct a procedure pr-add which inputs two polynomial representations, as described
; above, and which returns the polynomial representation of the sum of these.  Working test cases are required.

; The idea is to compute sums using representations, rather than polynomials.

; INSERT YOUR ANSWER HERE



; Problem 4  (20 points, with 15 points for the development process and proof)
; Design, implement and prove correct a procedure pr-mult which inputs two polynomial representations, as described
; above, and which returns the polynomial representation of the product of these.  Working test cases are required.

; The idea is to compute products using representations, rather than polynomials.


; INSERT YOUR ANSWER HERE


; Problem 5 (20 points, with 15 points for the development process and proof)
; Design, implement and prove correct the procedure p-eval described above. Here p-eval should input a polynomial expression,
; convert the constituent polynomials using poly-to-rep, carry out the operations pr-add and pr-mult as required by the expression on the representations of
; the component polynomials, and then convert back (using rep-to-poly) to deliver its returned value.  Working test cases are required. 


; INSERT YOUR ANSWER HERE



