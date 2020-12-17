

; CSc 335
; Fall 2019

; November 21

; Second Exam -- 1.75 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL CITYMAIL ADDRESS HERE:
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - working code with tests (out of 10 points)  10
;;;; Problem 1 - development/proof (out of 15 points  5

;;;; Problem 2 - working code with tests (out of 10 points)  na
;;;; Problem 2 - development/proof (out of 15 points) na

;;;; Problem 3 - working code with tests (out of 20 points)  7
;;;; Problem 3 - development/proof (out of 30 points)  5


;;;; Total  27
;;;; Letter Grade  D


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Everyone should solve the first two problems -- full points on these will yield a mid-C on the exam.

; The third problem, which makes use of solutions to the first two, is more challenging.  Solid progress towards a solution on
; this problem (in addition to solutions to the first two) is required for grades of B or A.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework: no vectors, no strings, no assignment.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and left in your backpack/briefcase/bag.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY, USING THE FORMAT LastnameFirstname.scm or LastnameFirstname.rkt

; TO ENSURE THAT THE MAILER DOES NOT REJECT YOUR FILE, USE SEMICOLENS FOR COMMENTS.  (The DrRacket comment boxes result in a graphics file being saved.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  


; Problem 1 (25 points) ; Write and certify an iterative procedure precedes which
;  - inputs a list of identifiers, such as '(d b e a c g f), in which each element that occurs in the list occurs just once
;  - inputs as well two identifiers from this list
;  - returns #t if the first of these identifiers occurs before the second in the list, and #f otherwise

; thus (precedes 'c 'e '(d b e a c g f)) = #f and (precedes 'd 'a  '(d b e a c g f)) = #t

;;;; INSERT YOUR ANSWER HERE

; I can just look for each of those elements in the 3rd parameter where
; i just return the position of the element then i just compare them
; first id must occur then the second thats why comparison of placement works. 

;Precedes:
;pre-condition: e1 and e2 are symbols, lis is not empty
;post-condition: #t or #f
;
;Position:
;pre-condition: e is a symbol, lis is not empty
;post-condition: an integer thats not negative
;
;GI : total work = work done + work remaining
;   : position = count_of_#_of_symbol_in_list_not_eq_to_e + symbols left in list to count 
;
;weak enough? (pre-condition + GI): assuming that the pre-condition holds, that means that the GI is also true
;                                 : because the list is not empty so there is work remaining left to be done
;                                 : and e is also a symbol so work will continue to be done if its the (car lis)
;                                 : is not eq to e
;
;Strong enough? (post-condition + GI): assuming the post-condition is true, at the last call of the function
;                                    : what is returned is the position of the symbol which is a non negative number
;                                    : so that means all the symbols were processed until e eq (car lis)
;                                    : count_of_#_of_symbol_in_list_not_eq_to_e + symbols left in list + 0 = return
;
;Preserved? : Assuming the pre-condition is correct then for some arbitrary call k, it will also hold true
;           : if arbitrary call k works then arbitrary call k+1 will also work such that if it is the last call, then
;           : post condition will be met
;           : count_of_#_of_symbol_in_list_not_eq_to_e(k) + symbols left in list to count(n-k) where is len of list
;
;termination? Program will terminate if the pre-condition is true. There will be a point in the function where the
;             post-condition will be met because if call k works then call k+1 works then maybe k+2, k+3 such that
;             at some k+nth call, the post condition will be met and termination will occur

(define (precedes e1 e2 lis)
  (define (position e lis pos)
    (cond ((null? lis) #f)
          ((eq? e (car lis)) pos)
          (else (position e (cdr lis) (+ pos 1)))))
  (if (> (position e1 lis 0) (position e2 lis 0)) #f #t))

(precedes 'd 'a  '(d b e a c g f))
        


; Problem 2 (25 points) ; Write and certify a recursive procedure check which inputs an sexp s and a list varlst
; of identifiers and decides whether s belongs to the class of fully
; parenthesized infix +-expressions fpip defined as follows:

;  var ::=  a | b | c | d | e | f | g
;  fpip ::= var | (fpip + fpip)

(define var '(a b c d e f g))

(define (fpip s-exp varlst)
  (define (flatten lis)
    (cond ((null? lis) lis)
          ((pair? lis) (append (flatten (car lis)) (flatten (cdr lis))))
          (else (list lis))))
  (define (member? lis x)
    (cond ((null? lis) #f)
          ((eq? (car lis) x) #t)
          (else (member? (cdr lis) x))))
  (define (main s-exp varlst)
    (cond ((null? s-exp) #t)
          ((eq? (car s-exp) '+) (fpip (cdr s-exp) varlst))
          ((eq? (car s-exp) '-) (fpip (cdr s-exp) varlst))
          ((member? var (car s-exp)) (fpip (cdr s-exp) varlst))
          (else #f)))
  (main (flatten s-exp) varlst))

(fpip '(a + (b + (c + d))) '())

;;;; INSERT YOUR ANSWER HERE


; Problem 3 (50 points) ; (READ ENTIRELY BEFORE STARTING) A fully parenthesized infix + expression will be said to be in left-normal
;  form if it is in the class described by

;    lfn ::= var | (var + lfn)

; where var is as given above.  Thus (b + (a + d)) is in left normal form, while
; ((b + a) + d) is not.

; A fully parenthesized infix + expression in left-normal form is said to be ordered with respect
; to a list varlist of variables if, read left to right, if its variables occur in the same order
; as they do in varlist.  Thus, if varlist = (a b c d), (a + (b + d)) is ordered with respect to
; varlist, while (a + (d + b)) is not.  Variables are allowed to occur multiple times -- for
; example, we will regard (a + (a + b)) to be ordered with respect to varlist, though
; (a + (b + a)) is not.  

; In this problem you are asked to implement and prove correct a program which uses commutativity
; and associativity of + to transform a fully parenthesized infix + expression e into left-normal
; form which is ordered with respect to the list of variables which occur in varlist.

; More specifically, your program is required to implement a process by which e is transformed one step
; at a time, with each step using one of three rules:  for arbitrary fpip expressions e1, e2 and e3,

;   (e1 + (e2 + e3)) may be rewritten as ((e1 + e2) + e3)
;   ((e1 + e2) + e3) may be rewritten as (e1 + (e2 + e3))

;   (e1 + e2) may be rewritten as (e2  + e1)

; Call your program ordered-left-normal.  It should

;   input an expression e belonging to fpip, and a list varlist of variables,

; and

;   return the expression e' belonging to lfn with variables ordered with respect to varlist, which is obtained from e by one-at-a-time
;   application of the three rules just shown. 

; You may assume that each variable of e occurs in varlist.  

; Example:  (ordered-left-normal '(((d + e) + f) + (a + (a + c)) '(a b c d e f g)) returns
; (a + (a + (c + (d + (e + f))))).

; Your program MAY NOT proceed by collecting variables and then constructing the desired output
; expression.  It MUST be implemented as a syntax-directed recursion on the input expression
; making explicit use of the single-step rewrites shown above.

; Suggestion: start by defining a data structure.

;;;; PUT YOUR ANSWER HERE






























