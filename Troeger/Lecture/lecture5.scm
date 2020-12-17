

; CSc 335
; Lecture 5 
; September 22 2020


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; class: 
;        look at ideas from Section 1.3 of Abelson and Sussman, folded  together with our ongoing
;        discussion of program proving

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Section 1.3  Formulating Abstractions with Higher-Order Procedures

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Section 1.3.1 Procedures as Arguments

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Consider 

(define (sigma a b)
  (cond ((> a b) 0)
	(else (+ a (sigma (+ a 1) b)))))

; As we have now developed careful proofs for both sigma and its corresponding
; iterative procedure, we will look at a sequence of procedures which increasingly
; generalize it.  In the course of doing so, we hope to shed some light on 
; higher order functions and their role in procedural abstraction.


; the first generalization is via a new parameter, term, which allows us to use
; the same code for many different computations

(define (sigma a b term)
  (cond ((> a b) 0)
	(else (+ (term a) (sigma (+ a 1) b term)))))

; if 

(define (term a)
  a)

(define (sum-integers a b)
  (sigma a b term))

; or

(define (sum-integers a b)
  (sigma a b (lambda (x) x)))


; we have again the original function. 


; but now we can do much more -- for example, instead of
; writing an entirely separate function to compute the sum of the squares
; of the integers from a to b, and another for the sum of the cubes of
; these integers, we can now reuse the sigma pattern:

(define (square x)
  (* x x))

(define (sum-squares a b)
  (sigma a b square))


(define (cube x)
  (* x (square x)))

(define (sum-cubes a b)
  (sigma a b cube))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is a good place to address the question of how one proves correct functions
; such as sigma which accept other functions as parameters.

; The short answer is this: one constructs a proof of (say) sigma which makes as few
; assumptions as possible about the parameter, term.  We can see from the code, for example, that
; term needs to be a function of one argument; if we assume that a is an integer, then
; clearly, term needs to allow integer arguments.  One would also want to say something
; about the value returned by the call (term a): for the current function, the best one
; can do is to say '(term a) must be a value which makes sense for +'.  

; Although we have ignored this issue up until now, the same thing needs to be stated
; for sigma itself: as part of the induction hypothesis, if one were being incredibly
; careful, one would say '(sigma a b term) returns a value which makes sense for +
; whenever gap(a,b) is less than ... '

; Continuing further in this direction would have us making assertions about +, >,
; and on and on through the implementation of recursion to the virtual memory system to ...

; It is understood, for all of our proofs, that at some reasonable point we stop -- at some
; point we say, simply, that the underlying systems are assumed correct.  In this sense,
; all of our proofs have been, implicitly, paremetrized proofs.  Now, with function
; parameters, the parametrization is explicit: we need to state clearly what we assume about
; the actual function parameters.

; Similarly, any termination argument we give for sigma must now be based on the assumption
; that the call (term a) terminates and returns a value. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; let's return to consider further enhancements of sigma.

; We can, for example, allow variation of the choice of
; the next term for inclusion in the summation

(define (sigma a next b term)
  (cond ((> a b) 0)
	(else (+ (term a) (sigma (next a) next b term)))))


; now we can obtain the original function as

(define (sum-integers a b)
  (sigma a (lambda (x) (+ x 1)) b (lambda (x) x)))


; and also


(define (sum-cubes a b)
  (sigma a (lambda (x) (+ x 1)) b (lambda (x) (* x x x))))



; we can even compute the sum

;    1         1          1 
;  -----  +  -----  +  -------  +  ...
;  1 * 3     5 * 7      9 * 11

; (which is known to converge to pi/8)

; as

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sigma a pi-next b pi-term))


(* 8 (pi-sum 1 1000))
(* 8 (pi-sum 1 5000))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a similar approach works for products

(define (prod a next b term)
  (cond ((> a b) 1)
	(else (* (term a) (prod (next a) next b term)))))


; allowing, for example

(define (factorial n)
  (prod 1 (lambda (x) (+ x 1)) n (lambda (x) x)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; this suggests that we abstract + and * to a more general 'combiner', so that sigma and prod
; can be obtained as special cases of a more general function, which we call
; accumulate


(define (accumulate combiner init a next b term)
  (cond ((> a b) init)
	(else (combiner (term a) 
			(accumulate combiner init (next a) next b term)))))


(define (sigma a b)
  (accumulate (lambda (x y) (+ x y)) 0 a (lambda (x) (+ x 1))  b (lambda (x) x)))


(define (prod a b)
  (accumulate (lambda (x y) (* x y)) 1 a (lambda (x) (+ x 1)) b (lambda (x) x)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; HOMEWORK A&S Exercises 1.29, 1.30, 1.31


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Section 1.3.2  The Relation between Let and Lambda

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Let allows one to bind variables as locally as possible to where they are to be used.

(define x 5)

(+ (let ((x 3))
     (+ x (* x 10)))
   x)


; let can be realized using lambda -- for example:

(+ ((lambda (x) (+ x (* x 10))) 3)
   x)


; One wants to be aware that a variable occurring in the definition part of a let will take its
; value from the context (environment) of the let.  In 


(define x 5)

(let ((x 3)
      (y (+ x 2)))
  (* x y))


; y will have the value 7 -- the outer x plus 2


; if one wants the second x to be bound by the first binding (x 3), one can use nested let, as
; follows

(let ((x 3))
  (let ((y (+ x 2)))
    (* x y)))


; perhaps more conveniently, use let*

(let* ((x 3)
       (y (+ x 2)))
  (* x y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HOMEWORK A&S Exercises 1.34, 1.37

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Introduction of Closures

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Section 1.3.4  Procedures as Returned Values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-addConstant x)
  (lambda (y) (+ x y)))


((make-addConstant 4) 5)


(define add4 (make-addConstant 4))

(add4 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; curried form of sigma

(define (curried-sigma term)
  (define (sum-term a b)
    (cond ((> a b) 0)
	  (else (+ (term a) (sum-term (+ a 1) b)))))

  sum-term)


((curried-sigma (lambda (x) x)) 1 10)


(define sum-of-squares
  (curried-sigma (lambda (x) (* x x))))


(sum-of-squares 1 10)


; contrast to

(sigma (lambda (x) (* x x)) 1 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (deriv f)
  (let ((dx .00000001))
    (lambda (x) (/ (- (f (+ x dx)) (f x))
		   dx))))

(define (cube x) (* x x x))


((deriv cube) 5)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; We briefly introduce (what Abelson and Sussman call)
; the environment model of evaluation, as one means for lending operational
; intuition to our understanding of closures.

; Please see Chapter 3 in the A&S text. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HOMEWORK A&S 1.41, 1.42, 1.43

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;














