; CSc 335 Section R
; Fall 2020

; December 15

; Second Part of Final Exam - time allocated is no less than 1 hour and 10 minutes

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SAVE YOUR EXAM AS Lastname.Firstname.PART2.scm

; QUESTIONS DURING THE EXAM: email me at dtroeger@ccny.cuny.edu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; INSTRUCTIONS (TWO parts, TWO mailings)

;; AFTER YOU RECEIVE THE EXAM

; start solving the exam questions in DrRacket as soon as you receive them

; FOR PART 2: NO LATER THAN 1745, stop writing, SAVE YOUR SOLUTION, AND EMAIL it to me at dtroeger@ccny.cuny.edu.  Your exam MUST be named
; with your name: Lastname.Firstname.PART2.scm, and it MUST include everything I have written here as well as your solution.

; Your email must be timestamped no later than 1750. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Scoring Template - Do NOT Erase!


;;;; Part 1:
;;;; Part 2:

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


; Part 2 of the Exam -- 20 points

; (10 points) Make sense of, and prove: "In any TLS expression without free variables, any reference to
; a variable x uses the closest enclosing binding of x."

; (10 points) Supposing that this statement is true, what properties of lexical scoping follow?  Give code
; examples to back up your points, and show, by drawing the environment table at strategic points in the
; evaluation, that your examples really do support your claims.  One of the examples should include the
; binding of a closure. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; THINK BEFORE YOU START CODING!!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; INSERT YOUR RESPONSE DIRECTLY INTO THE INTERPRETER GIVEN BELOW, INDICATING CLEARLY WHERE YOU HAVE MADE CHANGES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; HERE IS TLS, from Chapter 10 of The Little Schemer.

; TLS has been modified to deal with the default setting of drracket which blocks redefinition of built-in functions:

; first has been changed to myfirst
; second has been changed to mysecond
; third has been changed to mythird
; primitive? has been changed to myprimitive?

; and so on.  I may have missed one or two - if so, you can make the changes yourself.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; auxiliary functions

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define myfirst car)

(define mysecond cadr)

(define mythird caddr)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))



; environments implemented as tables


(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-table name
                                                (cdr table)
                                                table-f)))))))

(define extend-table cons)



(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (myvalues entry)
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names myvalues entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car myvalues))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr myvalues)
                                  entry-f)))))




(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define myvalues
  (lambda (entry) (cadr entry)))




; the top level of the interpreter

(define value
  (lambda (e)
    (meaning e (quote () ))))


(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


; supporting functions for the intepeter

; syntax-directed dispatch on expression

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote mul)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote))
          *quote)
         ((eq? (car e) (quote lambda))
          *lambda)
         ((eq? (car e) (quote cond))
          *cond)
         (else *application)))
      (else *application))))


; operational semantics -- the definitions of the action functions

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))


(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of mysecond)




(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))


(define initial-table
  (lambda (name)
    (car (quote ()))))


(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of myfirst)

(define formals-of mysecond)

(define body-of mythird)


; cond is a special form that takes any number of
; cond-lines ...  if it sees an else-line, it treats
; that cond-line as if its question part were true.

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))
                table))
      ((meaning (question-of (car lines))
                table)
       (meaning (answer-of (car lines))
                table))
      (else (evcon (cdr lines) table)))))


(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of myfirst)

(define answer-of mysecond)



(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)



(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))



(define *application
  (lambda (e table)
    (myapply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)




(define myprimitive?
  (lambda (l)
    (eq? (myfirst l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (myfirst l) (quote non-primitive))))



(define myapply
  (lambda (fun vals)
    (cond
      ((myprimitive? fun)
       (apply-primitive
        (mysecond fun) vals))
      ((non-primitive? fun)
       (apply-closure
        (mysecond fun) vals)))))


(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (myfirst vals) (mysecond vals)))
      ((eq? name (quote car))
       (car (myfirst vals)))
      ((eq? name (quote cdr))
       (cdr (myfirst vals)))
      ((eq? name (quote null?))
       (null? (myfirst vals)))
      ((eq? name (quote eq?))
       (eq? (myfirst vals) (mysecond vals)))
      ((eq? name (quote atom?))
       (:atom? (myfirst vals)))
      ((eq? name (quote zero?))
       (zero? (myfirst vals)))
      ((eq? name (quote add1))
       ((lambda (x) (+ x 1)) (myfirst vals)))
      ((eq? name (quote mul))
       (* (myfirst vals) (mysecond vals)))
      ((eq? name (quote sub1))
       (- (myfirst vals) 1))
      ((eq? name (quote number?))
       (number? (myfirst vals))))))


(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

; Part 2 of the Exam -- 20 points

; (10 points) Make sense of, and prove: "In any TLS expression without free variables, any reference to
; a variable x uses the closest enclosing binding of x."

; (10 points) Supposing that this statement is true, what properties of lexical scoping follow?  Give code
; examples to back up your points, and show, by drawing the environment table at strategic points in the
; evaluation, that your examples really do support your claims.  One of the examples should include the
; binding of a closure. 

;Development
;
; Lexical scoping follows the property that any inner function can use any outer function, any outer function however cannot use any
; inner function
;
; For Example:
; (def func1 x y)
;     (def func2 x y)
;         (def func3 x y
;
; func 3 has access to all func2 and func2 abut it can't be said the same about func1.
;
; The way lexical scoping works for the little schemer is that for every function call, a local variable table is used
; so the way im thinking about this is that since a local variable table is used, there is no consideratio nof free variables
; and only variables native to the function 
;
; (define func1 x table)
;       (def func2 x table)
;            (def func3 x table)
;
; where the table is local for each function while doing the process, they do not share the same table. Even if the function
; is to run recursively with a local variable table, it will only be local for the inner calls of the function and not for
; the outer
;
; Basis: All numbers, boolean values belong in our TLS so we can assume that they will evaluate correctly
;
; IH: For every expression that is an application, suppose a condition statement or lambda body, it will be evaluated correctly
;
; IS: Evalauted by case
;       i. lambda exp
;       ii. application
;
; i. For lambda bodies, there is always an expression (lambda x exp) following the rules of bound and unbound variables 
;     its said that if the variable x is bound then all instances of x is not free such that even if the expression of the lambda
;     body is an application, the identifier x will find the closest instance of x to bind
;
; ii. An application (e1, e2) can be identified as things like multiple cond statements so as evaluation of the cond statements are
;     being processed, as long as a variable is bound, then the first instance of evaluation of that variable will be bound
;     in the expressions e1 or e2