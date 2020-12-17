 ; CSc 335
; Lecture 4
; September 15 2020




; Solution to Homework 1, Exercise 3


(define (my-plus-version-1 a b)
  (if (= a 0)
      b
      (inc (my-plus-version-1 (dec a) b))))


; my-plus-version-1 is properly recursive, as you can see from either the syntax (the
; recursive call is guarded) or by expanding a typical call and noting the deferred calls
; to inc

; accordingly, and noting that the program works by decreasing a,  we construct a certification
; by giving an induction on a: that is, we show that for all integers a >= 0, and for all numbers
; b, the value of (my-plus-version-1 a b) is a + b 

; basis step: when a = 0, (my-plus-version-1 a b) returns b, which of course is 0 + b = a + b

; induction hypothesis: assume that the recursive call returns the correct value - that is, that
; (my-plus-version-1 (dec a) b) returns (a - 1) + b

; induction step: show that (my-plus-version-1 a b) returns a + b when a > 0.  Looking at the code,
; we see that (my-plus-version-1 a b), when a > 0, is just (inc (my-plus-version-1 (dec a) b)).
; By the induction hypothesis, and the definition of inc, this is 1 + ((a - 1) + b), or a + b


; termination: the integer a >= 0 is decremented by 1 with each recursive call - it will eventually
; reach 0, which will halt the program



(define (my-plus-version-2 a b)
  (if (= a 0)
      b
      (my-plus-version-2 (dec a) (inc b))))


; my-plus-version-2 is tail-recursive, or iterative, as you can see from either the syntax (the
; recursive call is unguarded) or by expanding a typical call and noting the absence of deferred
; calls to any function

; to make a first guess at the guess-invariant, we will in fact expand a typical call -- say

; (my-plus-version-2 4 5)
; (my-plus-version-2 3 6)
; (my-plus-version-2 2 7)
; (my-plus-version-2 1 8)
; (my-plus-version-2 0 9)
; 9

; we observe that the sum of the values of a and b is constant - in fact, that the sum is
; always the actual sum of a and b.  So our guess-invariant is quite reasonably taken to be


; actual-sum-of-original-values-of-a-and-b = a + b


; this is kind of awkward.  How about agreeing that we will use A to denote the original
; (input) value of a, and B to denote the original value of b.  Our convention will be
; that these values are constant -- some people refer to them as 'ghost variables'

; (eg, see J. C. Reynolds, The Craft of Programming)

; so now we can write our initial guess-invariant more simply as A + B = a + b



; we said at the end of Lecture 3 that one subjects guess-invariants to 3 tests, using these
; to refine the guess-invariant (if necessary) to produce a better approximation of the
; actual invariant.


; the first test:  is our guess invariant strong enough to imply the program's correctness
; on termination?  That is, assuming that the guess invariant is true each time the function
; my-plus-version-2 is called, does it imply that (my-plus-version-2 a b) returns A + B?  Well,
; have a look at the code.  The last time the function is called, a = 0, and then the value b
; is returned.  So if A + B = a + b, then A + B = 0 + b -- the value of b is precisely A + B.


; so the guess invariant passes our first test


; the second test: is our guess invariant true the first time the function is called?
; Again, we just examine the code: on first call, a = A and b = B.  There is nothing to
; prove!



; the third test: if our guess invariant is true the kth time the function is called,
; is it true on the (k+1)st call?  Everything is in the code:  if A + B = a + b on the
; kth call, then because the new parameter values for a and b are (a - 1) and (b + 1),
; respectively, it is clear that the guess invariant is true on the (k + 1)st call.


; thus we see that the guess invariant passes all three tests: it can be promoted to
; invariant


; all that remains to do is to argue that the program terminates - the argument is just
; like the one we used for the recursive version.




; this completes the solution of the homework problem



; it is worth emphasizing some additional points which arise naturally in discussing such
; correctness arguments


; first - do not ignore program testing.  It is at least as likely to make errors in constructing
; proofs as it is in constructing programs - a few judiciously chosen tests help increase our
; confidence in whatever code we write.  Let's revisit the example computed in class,
; (fact 1000): what exactly do we gain when we furnish our program with a proof?  Surely,
; we still cannot tell for certain that the 196th digit of the returned value is correct!

;; ; what we gain is this: if there is an error in the returned value, we can be quite sure that it is not
;; ; due to an error in our algorithm.  Literally dozens of subsystems contribute to the
;; ; successful evaluation of a program, from the operating system kernel to
;; ; the bignum libraries to the scheme compiler
;; ; itself: all we can do is hope that all of these have been certified as carefully as we certified
;; ; our own algorithm. 


; second, for any induction, an ellipsis (that is, ...) should never occur in the induction step.
; one gives away most of the software engineering benefit of this approach
; to program certification if one 'unwraps' the induction hypothesis.
; Returning to our discussion of factorial: one argues that
; (* n (factorial (- n 1))) returns n! because - by the induction
; hypothesis - (factorial (- n 1)) returns (n - 1)!.  Not because
; (factorial (- n 1)) expands to (- n 1) * (factorial (- n 2)), which
; expands to (- n 1) * (- n 2) * (factorial (- n 3)), which ...


; third, for iterative processes, one works to develop a guess at the invariant -
; call it the guess-invariant.  To refine this guess, check
; (a) whether the logical AND of the stopping condition and the guess-invariant
; implies that the program works, and (b) that the truth of the guess-invariant
; on the kth call of the procedure implies its truth on the (k+1)st call.
; If either (a) or (b) does not hold, revise the guess-invariant. Repeat
; until both tests succeed.  Once they do, check the hardest condition: that the
; truth of the guess-invariant on the kth call implies its truth on the (K+1)st
; call.  If this is so, then you are done; if not, you need to revise the guess-invariant
; yet again, and repeat the entire process.

; This is a heuristic process, with no guarantee that it will lead to the correct
; invariant.  You can rest assured, however, that every iterative process actually does
; have an invariant.

; Professional documentation for an iterative function could consist of its specification,
; its invariant, and an informal termination argument -- nothing more would be necessary

; Don't forget to include the termination argument!


; fourth, we say something more about specifications of the kind we shall use in CSc 335:

; a specification of the form

; {pre-condition} program {post-condition}

; is intended to be read as an implication -- If the pre-condition is satisfied, and IF
; the program terminates, then the post-condition will be satisfied when the program
; does terminate


; When we say that a program meets its specification, we mean only that this implication
; is true.  In particular, were one to violate the precondition, then all bets are off:
; you cannot sue me for trying to use my integer division program to divide a by 0 if
; this program satisfies a specification which includes the input restriction that
; the denominator be non-zero.



; more, one wants to make the precondition as general (weak) as possible, since to do
; otherwise may unnecessarily restrict the application of the program.  In our addition
; programs, above, there is for example no reason to insist that the input b be positive,
; or that it be an integer.


; practice: which of these specifications would be better?

; {true} program {your post-condition}
; {false} program {your post-condition}


; do you see from your understanding of implication (recall that F ==> Q is always true)
; that the second specification imposes no restrictions at all on the program?

; do you see that the first specification imposes no restrictions at all on the input?






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Section 1.2.2 in Abelson and Sussman

; Tree Recursion

(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1)) (fib (- n 2))))))


; draw the tree-recursive process for (say) (fib 4)

; use induction to prove that (fib n) computes the nth fibonacci
; number

; for practice with induction, show that the number of times (fib n)
; computes (fib 0) or (fib 1) is (fib (+ n 1))

; observation: one can use Binet's formula (Google it!) to show that (fib n) requires time
; which grows exponentially with n


; sometimes we can see how to replace tree recursions by iterative processes


; consider

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))



; use an invariant-based induction to prove that (fib n) computes the
; nth fibonacci number


; a good way to get started on finding a guess-invariant is to ask yourself: what role is fulfilled by
; each variable?

; that is, it is often useful to enumerate the _design roles_ of the variables

; some experimentation is usually required to find design roles
; which work

; let's observe that b is supposed to be the nth fibonacci number when the program terminates
; let's observe that the program terminates when count = 0
; let's observe that count is initially n, and b is initially 0

; can you spot a model which fits these data?  how about

; b is the (n - count)th fibonacci number

; certainly this fits as far as we have gone.  what about a design role for the parameter a?

; let's observe that as the program runs, a is always the fibonacci number which comes after b

; so we take as design role for a:

; a is the (n + 1 - count)th fibonacci number


; sometimes, as here, the guess invariant can be given simply as the logical AND of the design roles

; (you should check that this works!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; is the proof easier if we change the design role of count? maybe have it
; count up rather than down? Try this for homework! Here is some code



(define (fib n)
  
  (define (fib-iter a b count)
    (if (= count n)
	b
	(fib-iter (+ a b) a (+ count 1))))

  (fib-iter 1 0 0))


; now the design roles are:

; b is the nth fibonacci number (remember that we start with 0)
; a is the (n + 1)st fibonacci number


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; still, we might ask whether we really need to compute the (N+1)st fibonacci number
; in order to return the Nth fibonacci number.

; what about a design such as this?

(define (fib n)

  (define (fib-iter curr prev count)
    (cond ((= count n) curr)
	  (else (fib-iter (+ curr prev) curr (+ count 1)))))

  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (fib-iter 1 0 1))))


; what are the design roles of curr and prev?  what is the invariant?  is this version correct?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





; observe that the last versions of fib are linear iterations




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; tree recursions are inefficient, but nonetheless useful because
; they are relatively easy to design.  if one had a tool which
; automatically transformed tree recursions to more efficient
; procedures which compute the same result, one might have a
; useful (scalable) way of approaching otherwise difficult
; problems


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Leading up to Section 1.3.1 in Abelson and Sussman - and two somewhat more involved proof examples

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Consider the summation procedure sigma:

(define (sigma a b)
  (cond ((> a b) 0)
	(else (+ a (sigma (+ a 1) b)))))


; this is a recursive program, but the induction is a bit different from what we have seen so far
; in that we will induct on the size of the gap between a and b:


;   -----------------------------------------------------------
;   |              a                            b             |
;   ----------------------------------------------------------
;                  | <--------gap(a,b)--------->|


; more specifically, we will take gap(a,b) = b - a if b >= a, and 0 otherwise.

; We will work out the details in class, but note here that the basis step - for gap(a,b) = 0 - has two cases:

; if (> a b), then the code immediately returns 0, which is the correct value for the sum of all values between
; a and b when a > b

; if (= a b), then the procedure returns (+ a 0) = a, again as it ought.


; for the induction step, observe that gap (a + 1, b) is less than gap (a, b) when a < b




; Now consider an iterative version

(define (sigma a b)

  (define (iter a result-so-far)
    (cond ((= a b) result-so-far
	  (else (iter (+ a 1) (+ (+ a 1) result-so-far))))))

  (cond ((> a b) 0)
	(else (iter a a))))


; with guess-invariant for iter as follows:

; a <= b AND result-so-far = sum of all integers i from A up to a, A <= i <= a, where A is the
; original value of the parameter a

; you can check the invariant conditions; once done, you need to check termination.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; for home study

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; it is instructive to view several alternatives, especially if we take the tact of trying to discover
; what (if anything) is wrong with each one


; for example, this version simplifies the code following the internal definition - do we really need
; that second cond?


(define (sigma a b)

  (define (iter a result-so-far)
    (cond ((> a b) result-so-far)
	  (else (iter (+ a 1) (+ a result-so-far)))))

  (iter a 0))

; as guess-invariant, we might take:  result-so-far is the sum of all integers i,
; A <= i <= a.  Here A is the initial value of the parameter a.

; is there anything wrong with this?

; well, when we begin running this through the first of our tests (is the invariant
; true the first time iter is called?), we note a problem: result-so-far is 0,
; while the sum of all integers i, A <= i <= a, is A when a = A.


; can we fix this problem by revising the guess-invariant?   Try
;   result-so-far is the sum of all integers i, A <= i < a.

; Now when a = A, this sum is 0, because no integers satisfy A <= i < A, and the empty sum is 0 by definition

; Looking good! But while the new guess-invariant passes the first test, it fails the second (is the invariant
; strong enough to imply that the program is correct when it terminates?): the problem
; is that all we know from the code on termination about a is that it is greater than b.  It could
; be b+1, or b+2, or ...

;; [I can hear you saying: we know in fact from the history of values in b that its final value is
;; b + 1, and so this version is correct.  But just as we do not want to unwind a recursion, we also
;; do not want to allow variables' histories into our correctness checking.  Think of the increase
;; in complexity which would result from this! We want to use what I will call 'static logic', and
;; not 'dynamic logic': we want to keep time out of the picture.]

; so we add another clause to the guess invariant: a <= b+1.  Now, on termination, we
; would have a > b && a <= b+1, which gives a = b+1.  And then result-so-far, the
; sum of all integers i, A <= i < b+1, would in fact be what we want: the sum of all
; integers i, A <= i <= b.


; adding this clause, however, triggers another problem: how do we know a <= b+1 when
; iter is first called?  At the moment, we do not know this:

; pre: a and b are integers
; post: the sum of all integers i, a <= i <= b, is returned

; not wanting to modify the precondition (why?), we opt instead to modify the code

; guess-code-version-2:


(define (sigma a b)

  (define (iter a result-so-far)
    (cond ((> a b) result-so-far)
	  (else (iter (+ a 1) (+ a result-so-far)))))

  (cond ((> a (+ b 1)) 0)
	(else (iter a 0))))


; guess-invariant-version-2

; result-so-far is the sum of all integers i, A <= i < a
; AND
; a <= b + 1


; this all seems to work (you should work through the details), but there is still
; something distasteful about calling iter TWICE when a = b.  I mean, the answer is just
; a, right?



; maybe we could try  guess-code-version-3

(define (sigma a b)

  (define (iter a result-so-far)
    (cond ((= a b) (+ a result-so-far))
	  (else (iter (+ a 1) (+ a result-so-far)))))

  (cond ((> a b) 0)
	(else (iter a 0))))


; with guess-invariant-version-3

; result-so-far is the sum of all integers i, A <= i < a
; AND
; a <= b


; this last version leads naturally enough to the cleaner version we started with.  The entire
; sequence of designs illustrates a key point about code development in this setting: one uses the logic
; side-by-side with the code in a mutually supportive refinement process.  We view programs as
; fluid systems, with deeply intertwined -- and equally visible -- code and logic components. 


; you should work out arguments for the last two versions of sigma! 



