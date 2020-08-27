

; csc 335 Lecture 1
; August 27 2020


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 


; from Section 1.1 of Abelson and Sussman

; 1.1.1 Expressions

; expressions are evaluated

; some expressions are primitive

; numbers are primitive expressions

486

12.01


; another kind of expression is a primitive procedure

+

; arithmetical expressions are formed from numbers and primitive procedures


; arithmetical expressions are fully parenthesized, with the operator in the
; prefix position


(+ 2 3)

(+ 2 3 4 5)

(+ (* 4 5) 3)

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))



; there are many other kinds of expressions, which we will mostly ignore for now

"Hello world"

(string-append "Hello" " world")



; 1.1.2 Naming

; we can name values, and then use them

(define size 2)

(+ 5 size)



(define pi 3.14159265389793)

(define radius 10)

(* pi (* radius radius))



; 1.1.3  Evaluating Combinations

; to evaluate a combination, do the following

;; 1.  evaluate the subexpressions of the combination

;; 2.  apply the procedure that is the value of the leftmost
;;     subexpression (the operator) to the arguments that are the
;;     values of the other subexpressions (the operands)



; use this rule on the third example above:

; (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))


; clearly, this expression can be cast as a tree, with primitive
; expressions (in this case, numbers) as leaves.

; to halt the recursion implicit in this evaluation rule, scheme stipulates
; that

; the values of numerals are the numbers they name

; the values of built-in operators are the machine instruction sequences
; that carry out the corresponding operations

; the values of other names are the objects associated with those names by
; define



; 1.1.4  Compound Procedures

; functions are values, too

(lambda (x) (* x x))

((lambda (x) (* x x)) 2)

; function values can be named

(define square
  (lambda (x) (* x x)))

(square 2)

; perhaps more conveniently, but with some loss of clarity, we can omit 'lambda'

(define (square x) 
  (* x x))

(square 2)


; some names are local, while others are global

(define x 3)

(define (add-x y)
  (+ x y))


; here, x is global and y is local


; that is, y is unknown outside the body of the function add-x

; indeed, if we

(define y 17)

(add-x 4)

; and then again type

y

; we will see that the value of (the global) y is unchanged - it is still 17





; in this next example, we note that a function body may refer to a variable
; (in this case, z) which has not been defined -

(define (add-z y)
  (+ z y))

; we cannot apply add-z until z is defined, however




; function names can occur in other functions' bodies, too 


(define (cube x)
  (* x (square x)))


; here, square is global


; one last point about square: it is generic, at least to the point of 
; accepting inputs whih make sense for *

(square 2)

(square 2.1)


; function names can also occur in their own function bodies, as we
; will see very soon.

; functions with this property are said to be recursive


; functions may occur in other functions - for example

(define (sum-of-squares x y)
  (+ (square x) (square y)))


; we can use sum-of-squares as a building block in constructing
; further procedures

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))


(f 5)


; functions can also have 0 arguments

(define (one) 1)

; equivalently

(define one (lambda () 1))


; functions need not use all of their arguments 

(define (two x y z) 2)

; equivalently 

(define two (lambda (x y z) 2))




; 1.1.5 The Substitution Model for Procedure Application

; To apply a compound procedure to arguments, evaluate the body of the
; procedure with each formal parameter replaced by the (value of) the
; corresponding argument

; to illustrate:

; (f 5)

; ((sum-of-squares (+ a 1) (* a 2)) 5)

; (sum-of-squares (+ 5 1) (* 5 2))

; (sum-of-squares 6 10)

; (+ (square 6) (square 10))

; (+ (* 6 6) (* 10 10))

; (+ 36 100)

; 136




; 1.1.6 Conditional Expressions and Predicates

(define (myabs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	(else (- x))))

(myabs -4)

; another way

(define (myabs x)
  (cond ((< x 0) (- x))
	(else x)))

; another way

(define (myabs x)
  (if (< x 0)
      (- x)
      x))


; >, =, < are primitive predicates

; Scheme also supplies logical connectives such as and, or, not

(define x 6)

(and (> x 5) (< x 10))

(not (> x 4))

#f

(not #f)

(= x 6)

; and so on

; note that cond and if do not obey the standard evaluation rule -

; (cond (p1 e1) (p2 e2) ... (pn en) (else E)) is evaluated as follows:
;   evaluate p1 -- if #t, then evaluate e1 and return this value
;   otherwise evaluate p2 -- if #t, evaluate e2 and return this value
;   otherwise evaluate p3 -- if #t, evaluate e3 and return this value

;   and so on.  If none of the pi is #t, evaluate E and return this value


; similarly, (if p1 e1 e2) evaluates e1, but not e2, if p1 is #t; it
; evaluates e2, but not e1, if p1 is #f


; cond and if are said to be special forms

; can you see that define is also a special form?  explain!


;;;; You should read Sections 1.1.1 through 1.1.6, and solve Exercises
;;;; 1.1 through 1.5, in Abelson and Sussman.

;;;; To prepare for the next class, read Sections 1.1.7 and 1.1.8, as well
;;;; as Section 1.2.1.

