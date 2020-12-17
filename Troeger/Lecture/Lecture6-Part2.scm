

; CSc 335
; Lecture 6 - Part 2
; October 7 2020




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.11 - as given in the text

; a function f is defined by the rule that f(n) = n if n < 3 and
; f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) if n >= 3.  Write a procedure that
; computes f by means of a recursive process.  Write a procedure that
; computes f by means of an iterative process.

; here is a procedure that computes f by means of a recursive process

(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))



; some thought is required to compute f by means of an iterative process

; design roles


; w = f(m)
; x = f(m-1)
; y = f(m-2)
; z = f(m-3)


; idea for iterative computation

; w <- x + 2y + 3z
; x <- w
; y <- x
; z <- y

; where the updates, just as parameter updates in scheme,  do not interfere with one another


(define (f-iterative n)
  
  (define (f-iter m w x y z)
    (cond ((= m n) w)
	  (else (f-iter (+ m 1) (+ w (* 2 x) (* 3 y)) w x y))))

  (cond ((< n 3) n)
	(else (f-iter 3 (+ 2 (* 2 1) (* 3 0)) 2 1 0))))



; the proof consists in showing (i) that the 'idea for iterative computation', above, is
; correct, and (ii) that f-iter implements this idea correctly.  For the latter, note that
; the parameter updates are computed using the old value of m


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework: A&S Exercise 1.12

; pascal's triangle -- can we compute the entries from their 
; indices?

;                         1
;                      1     1
;                   1     2    1 
;                1     3     3   1
;            1      4     6    4   1
; and so on

; suppose we assign row indices in the usual way, and column
; indices for slanted columns  -- so the first 3, for example,
; has row index 4 and column index 2.

; we want to write a scheme program pas of two arguments row, col
; and which returns the pascal triangle entry in that position.

; row, col must of course be suitably constrained -- col <= row
; and of course, both must be positive

; design and prove correct a program which solves this problem

(define (pas row col)
  (cond ((= col 1) 1)
	((= row col) 1)
	(else (+ (pas (- row 1) (- col 1))
		 (pas (- row 1) col)))))



; An interesting aspect of the proof of pas surfaces as soon as we ask the most
; basic question pertaining to any induction.  Namely, what, exactly, are we inducting
; on?  As the function has two arguments, it is natural to think that we need to induct
; on both of them.  The induction hypothesis would perhaps be something like:  assume
; (pas r c) works correctly for all row indices r which are less than the current row
; index and all column indices c which are less than the current column index.  But
; this cannot work, given that the second call to pas uses the current column index.

; Noting from the triangle figure itself, above, that all entries in row r are determined
; by those in the previous row, we are led to try a simpler induction hypothesis:  assume
; that (pas r c) works correctly for all row indices which are less than the current row,
; so long as the column index makes sense for that row.  In short: assume that the entire (r-1)st
; row can be correctly computed. 

; Now the induction step is almost as straightforward as one could hope.  The only tricky part
; has to do with the need to show that the column indices for the two recursive calls
; make sense for the previous row.  To pull this off, we want to make explicit the
; requirement that a column index makes sense: as seen in the problem description, we need
; 1 <= col <= row.

; Does this condition hold for both of the recursive calls?  That is, is it the case that
; col <= row - 1 when those calls occur?  If not, we have illegal calls, and must begin again.

; Of course it holds for the first call: (pas (- row 1) (- col 1)).  What about the second,
; (pas (- row 1) col)?  Argue as follows: because (= col row) is false (look at the
; code), col <= row (from the IH) implies col < row.  Hence col <= row - 1, and we have what
; we need.

; Thus we can state that both recursive calls work correctly, and hence that (pas row col)
; returns the correct result.

; To summarize: you can prove pas correct by inducting on the row index, using induction
; hypothesis

;   (pas r c) works for all row indices r less than the current r index and all c which
;   satisfy 1 <= c <= r.

; I will leave the remaining details (the basis step, the termination argument) to you.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; what about an iterative computation for the entries in Pascal's triangle?  I think the
; way to proceed on this is to fall back on your background in discrete mathematics, as
; it seems to be hard to find a transformation analagous to the one applied to transform
; recursive Fibonacci to iterative Fibonacci.  

; you will recall that the (n,k) entry in Pascal's triangle - row index n, column index k -
; is precisely the quantity n choose k, i.e,

;                                      -- --
;                                      | n |
;                                      | k |
;                                      -- --

; As you know from CSc 104, this is just n! / (k! (n-k)!).  Since we know how to compute
; factorial iteratively, we can certainly use this formula to produce an iterative
; function for n choose k.

; A more efficient formula removes the redundant multiplications -- you can check that
; n! (k! (n - k)!) is equal to the product as i ranges from 1 to k of the terms
; (n + 1 - i) / i.

; We previously developed an iterative multiplication program for arbitrary terms, so this
; also gives an iterative method for computing the coefficients.  


; Another approach is to replace the recursion with what amounts to a for-loop: one keeps
; track of the last row computed (perhaps in a parameter whose values are lists) and then
; computes the next row from the previous row in the obvious way.  An apparent problem with this is
; that it fails the 'constant space' criterion which characterizes linear recursions, as
; it is evident that the rows increase in size as the row index increases.  On further
; thought, however, this growth in parameter size occurs also for the iterative factorial
; procedure: the parameter factorial-so-far takes up more space as it becomes larger.  So
; we have to ask: which 'space' are we referring to when we say that a recursive program
; runs in constant space?  For now, let us agree that we refer to the size of the stack of
; deferred calls: recursive factorial piles up a stack of deferred calls, while iterative
; factorial does not.  Similarly, the pascal coefficient program which keeps track of just
; the last row of the triangle does not build up a stack of deferred calls. 

; Once we have studied lists, we should come back to the problem of implementing the pascal
; coefficient problem iteratively, in the manner I have just described.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This mention of binomial coefficients highlights a deficiency of the code presented above -- it
; uses 1-based indices, which are not in line with the indices n and k as customarily used in
; the phrase "n choose k".  Can you rewrite the program so that one would enter row index 3
; and column index 1 to obtain the first 3 in the table?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Exercise 1.29 from Abelson & Sussman


;use the definition of sum given in the text
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

; here is the integration function given in the text -
; note that with each call, a is incremented by dx --
; we would likely have non-termination if we were
; to replace the stopping test (> a b) in sum by
; (= a b), due to the nature of floating point
; addition.

(define (integral f a b dx)
   (define (add-dx x) (+ x dx))
   (* (sum f (+ a (/ dx 2.0)) add-dx b)
      dx))

; test case

(define (cube x) (* x x x))

(integral cube 0 1 0.01)



; now for simpson's rule as described in the exercise.  note
; that I have used 0 and n as the initial values of the sum
; parameters a and b, and that the increment is by 1 with
; each call.

; an advantage of this approach is that the gap used to
; demonstrate termination and correctness in sum is always
; integer-valued.

; a drawback is that we have to repeatedly multiply h by
; the counter, k, rather than just add h at each step.

; it would be nice to have a workaround for this, other
; than going back to base the stopping condition on real values.  Can anyone
; see a way to accomplish this?  Hint: think of multiplication by
; repeated addition.  
    
;assumes that n is even
(define (simpson f a b n)
  (simp-aux f a b n (/ (- b a) (* n 1.0) )))

(define (simp-aux f a b n h)
  (define (term k)
      (cond ((= 0 k) (f a))
        ((= n k) (f (+ a (* k h))))
        ((odd? k) (* 4 (f (+ a (* k h)))))
        ((even? k) (* 2 (f (+ a (* k h)))))
        ))
  
  (define (next k)
    (+ k 1))

  (* (/ h 3.0)
     (sum term 0 next n))

  )
    
    
(simpson cube 0 1 100)

(simpson cube 0 1 1000)

(simpson cube 0 1 10000)


; discussed in class: what is our proof obligation for the function simpson?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Exercise 1.31 from Abelson and Sussman

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))


;for n >= 0
(define (factorial n)
  (define (id x) x)
  (define (incr x) (+ x 1))
  
  (iter-prod id 1 incr n))


;now for an iterative product
(define (iter-prod term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))


;now for Wallis' product
(define (wallis n)
  (define (term k)
    (cond ((odd? k) (* 1.0 (/ (+ k 1) (+ k 2))))
          (else (* 1.0 (/ (+ k 2) (+ k 1))))))
  (define (next k)
    (+ k 1))
  (iter-prod term 1 next n))


(* 4 (wallis 1000000))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.37

; iterative version

(define (cont-frac n d k)
  (define (cont-iter result count)
    (if (= count 0)
        result
        (cont-iter
         (/ (n count) (+ (d count) result))
         (- count 1))))
  (cont-iter (/ (n k) (d k))
             (- k 1)))

; test with golden ratio

(cont-frac 
 (lambda (i) 1.0)
 (lambda (i) 1.0)
 1000000)


; guess-invariant: result =

    ;;  N_(count+1)
    ;; ------------------------
    ;;  D_(count+1) + N_(count + 2)
    ;;                ------------------------------
    ;;                D_(count + 2) + N_(count + 3)
    ;;                                ---------------------------
    ;;                                D_(count + 3) + 
    ;;                                               .
    ;;                                                .
    ;;                                                 .
    ;;                                                  N_(k)
    ;;                                                  -----
    ;;                                                  D_(k)

; so now when count = 0, the first numerator included in result is
; N_(1).

; rest of checking is left for class.




; recursive version

(define (cont-frac-rec n d k)
  (define (cont-aux count)
    (if (= count k)
        (/ (n count) (d count))
        (/ (n count)
           (+ (d count)
              (cont-aux (+ count 1))))))
  (cont-aux 1))


(cont-frac-rec 
 (lambda (i) 1.0)
 (lambda (i) 1.0)
 1000000)


; the guess induction hypothesis: (cont-aux j) works correctly for
; all j > i, so long as j <= k.

; the basis step is for j = k

; the details can be checked in class.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; The next problem illustrates how one can use the substitution model
; and function application in calculating the value of functional expressions.  The
; exercise gives insight into the 'machine code of the lambda calculus', and it is also
; a good way to test your understanding of the substitution model, function application,
; and first-class functions.  




; Exercise 1.41


; here is the definition of the function double

(define (double f)
  (lambda (x) (f (f x))))

; here is the definition of the function add-one

(define (add-one x)
  (+ x 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; our task is to calculate (better: explain) the value of
; (((double (double double)) add-one) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; We apply the definition strategically to transform the original
; call into something we can evaluate at a glance

; each of these lines has the same value as the one beneath it,
; as you can readily check by evaluating them

; each line is obtained from its predecessor by either
; expanding an occurrence of double in accord with its definition,
; or by applying a lambda form to its argument

(((double (double double)) add-one) 5)


; our first expansion amounts to replacing
;   (double f) by (lambda (x) (f (f x)))
; in accord with the definition of double, where f is a temporary abbreviation for (double double)


(((lambda (x) ((double double) ( (double double) x)))  add-one) 5)

; next we apply the lambda form to its argument - that is, we replace x in the body of
; the lambda by add-one

(((double double) ( (double double) add-one)) 5)

; now expand the second occurrence of (double double), again using the definition of double

(((double double) ( (lambda (x) (double (double x))) add-one)) 5 )

; and apply the lambda form

(((double double) (double (double add-one))) 5)

; and expand now the first occurrence of (double double)

(((lambda (x) (double (double x))) (double (double add-one))) 5)

; substitute for x -- ie, apply the lambda form to its argument

((double (double (double (double add-one)))) 5)

; and finally we are in position to compute the value

; clearly, the value is 21


;;; ... whew!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; exercise 1.42


(define (compose f g)
  (lambda (x) (f (g x))))

(define (addone x)
  (+ x 1))

(define (square x)
  (* x x))

((compose square addone) 3)

; what is assumed about f and g?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; exercise 1.43

(define (repeated f k)
  (if ( = k 1)
      f
      (compose f (repeated f (- k 1)))))


(define (compose f g)
  (lambda (x) (f (g x))))


(define (square x)
  (* x x))


((repeated square 2) 5)


; I leave the iterative solution to you.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
