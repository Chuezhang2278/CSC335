
; csc 335 Lecture 2
; September 1 2020


; we continue with Chapter 1 of Abelson and Sussman

; it is interesting to note that cond and if forms can be evaluated
; by themselves - they do not need to be embedded in a function

(define x 6)

(cond ((> x 6) 1)
      ((< x 6) 2)
      (else 0))


(if (= x 6) 2 3)

; and even such expressions as

(+ (cond ((> x 6) 0)
	 (else 1))
   2)



; let's have another example or two

; first, we observe that the substitution model of evaluation described 
; above allows for combinations whose operators are compound expressions. 

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; the first item in the list -- namely (if (> b 0) + -) -- is equal 
; to + if (> b 0), and otherwise -

; thus 

(a-plus-abs-b 4 -5)

; is 9




; next, we write a program to compute the sum of the squares of the two larger
; of three inputs

(define (sum-square-of-two-largest x y z)
  (cond ((= x (min x y z)) (+ (square y) (square z)))
	((= y (min x y z)) (+ (square x) (square z)))
	(else (+ square x) (square y))))

; or, more elegantly, using the special form let, 

(define (sum-squares-of-two-largest x y z)
  (let ((m (min x y z)))
    (cond ((= m x) (+ (square y) (square z)))
	  ((= m y) (+ (square x) (square z)))
	  (else (+ (square x) (square y))))))



; let defines a local variable, with scope just the body of the
; function sum-squares-of-two-largest


; another way to obtain a local variable would be to use define as follows

(define (sum-squares-of-two-largest x y z)
  (define m (min x y z))
  (cond ((= m x) (+ (square y) (square z)))
	((= m y) (+ (square x) (square z)))
	(else (+ (square x) (square y)))))


; (As we shall see, R5RS is not as flexible with define as it is with let.)





; An alternate model for procedure evaluation

; according to the description of evaluation given above, the interpreter
; first evaluates the operator and operands and then applies the resulting
; procedure to the resulting arguments

; this is called applicative order, or call-by-value

; another possibility: hold off on evaluating the operands until their
; values are actually needed.

; this is called normal order, or call-by-name



; let's recall the earlier example

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

; using applicative order, the evaluation proceeds as

; (f 5)

; ((sum-of-squares (+ a 1) (* a 2)) 5)

; (sum-of-squares (+ 5 1) (* 5 2))

; (sum-of-squares 6 10)

; (+ (square 6) (square 10))

; (+ (* 6 6) (* 10 10))

; (+ 36 100)

; 136





; using normal order, on the other hand, we obtain the same value 
; as before, but the computation proceeds differently:

; (f 5)

; ((sum-of-squares (+ a 1) (* a 2)) 5)

; (sum-of-squares (+ 5 1) (* 5 2))

; (+ (square (+ 5 1)) (square (* 5 2)))

; (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))

; (+ (* 6 6) (* 10 10))

; (+   36      100)

; 136




; applicative and normal order evaluation do not always give the same result

; consider

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; what will happen if (test 0 (p)) is evaluated using applicative order?  why?
; what will happen if it is evaluated using normal order?


; you can stop an infinite loop under DrRacket by using the red Stop button
; at the top right of your screen




; remark

(if #t 0 (p))

; returns 0 -- because if is a special form, the infinite loop (p) is never evaluated

; to drive this point home, suppose that instead of if, we defined our own function, new-if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(new-if (> 2 0) 3 4)

(new-if (> 2 3) 3 4)

; this seems to work!  But should we try ...


(let ((x 0))
  (new-if (= x 0) 
      0
      (p)))


; we find that we have an infinite loop, precisely because the standard evaluation
; rules apply to the user-defined function new-if: new-if is not a special form




; Let us look next at a somewhat longer program in scheme,
; implementing Newton's method for computing square roots,
; to give an idea how one can combine procedures to produce
; larger procedures


; primary interface 


(define (sqrt x)
 (sqrt-iter 1.0 x))

; main auxilliary function

(define (sqrt-iter guess x)
 (if (good-enough? guess x)
     guess
     (sqrt-iter (improve guess x)
                 x)))

; the stopping test

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     .001))

; derive improve from f(x) = (x^2 - a) using basic calculus  

(define (improve guess x)
 (average guess (/ x guess)))


; other auxilliary functions for sqrt

(define (average x y) (/ (+ x y) 2))

(define (square x) (* x x))


(define (abs x)
 (if (>= x 0)  
     x
     (- x)))




; You might want to play with this: what happens when the initial guess 1.0
; is replaced by -1.0?  By 25.0?  


;;;;;;;;;;


; comments on interactive development style

; indication of basic tests

; (sqrt 5.0)
; (square (sqrt 5.0))

; why does (sqrt 5) -- as opposed to (sqrt 5.0) -- work?  what if we changed the 1.0
; to 1 in the call of sqrt-iter from sqrt?



;;;;;;;;;;


; once all the pieces are working, one can collect
; them into a kind of module - local function definitions! - and
; then use lexical scoping to remove unnecessary parameters


(define (sqrt x)

  ; main auxilliary function

  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))


  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       .001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (average x y) (/ (+ x y) 2))

  (define (square x) (* x x))

  (define (abs x)
    (if (>= x 0)  
	x
	(- x)))

 (sqrt-iter 1.0))
 


; (sqrt 2)
; (square (sqrt 2))

; (sqrt 91)
; (square (sqrt 91))



;;;;;;;;;;


; Have you noticed that we have nowhere mentioned assignment?

; All we are doing is defining and composing functions - are
; you surprised?


;;;;;;;;;;







; in preparation for the next class, please read, in Abelson and Sussman, Section 1.2.1
