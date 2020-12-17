

; CSc 335
; Spring 2018

; April 17

; Second 2 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 10 points)
;;;;           - proof, and synergy of proof and code (out of 10 points)

;;;; Problem 2a - code (out of 10 points)
;;;; Problem 2b - code (out of 10 points)

;;;; Problem 3 - code (out of 25 points)
;;;;           - proof, and synergy of proof and code (out of 10 points)

;;;; Problem 3b EXTRA CREDIT - code (out of 10 points)

;;;; Problem 4a - code and proof and synergy (out of 10 points)
;;;; Problem 4b - code and proof and synergy (out of 15 points) 

;;;; Total
;;;; Letter Grade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework: no strings, no vectors, no assignment.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  


; Problem 1 (20 points) Write a certify an iterative program, using lists, named
; nth-row-of-pascal-triangle which inputs a positive integer n and which outputs
; the nth row of the pascal triangle,

; Thus (nth-row-of-pascal-triangle 3) returns the list (1 2 1), and
; (nth-row-of-pascal-triangle 4) returns (1 3 3 1).  Your program should of course
; work for other positive integers n -- for example, (nth-row-of-pascal-triangle 10)
; should return (1 9 36 84 126 126 84 36 9 1)

; (The main program should be iterative, but you are free to use properly recursive
; auxiliary functions if it seems useful to do so.)

; Be sure to include (WORKING) tests of your function.  

;;;; INSERT YOUR ANSWER HERE
(define (produce-next-row lst)
  (define (aux lst)
    (cond ((null? lst) '())
          ((= 1 (length lst)) (cons '1 '()))
          (else (cons (+ (car lst) (cadr lst)) (aux (cdr lst))))))
  (append '(1) (aux lst)))
;proof:
; basis: a null list will produce another null list.
; IH: this function will return a list base on the input list. The returned list will the next row of that input list in a pascal triangle.
; IS: We know that a row of pascal triangle is based on the previous row. It get all the element by adding the (row-1, col-1) and (row-1, col).
;     we already have the previous row and we know that the sum of first two element of previous row give us second element in the next row. By using
;     this method we can get the second element of our next row, then cdr the previous row. Again adding frist two element of prev row will give us
;     the next element in the next row.
; Termination: When the previous row is left with a single element, we know that we reach the last element of next row. The last element in a row of
;              pascal tri is a '1' therefore we return a 1 and terminate the function. When the function terminate we have to append a 1 to the list
;              it produce because we only start building the next row from the second element.
(define (nth-row-of-pascal-triangle n)
  (define (iter row k)
    (cond ((= k n) row)
          (else (iter (produce-next-row row) (+ k 1)))))
  (iter '() 0))
;(nth-row-of-pascal-triangle 1)
;proof:
; GI: row will be the kth row of the pascal tri
; first call: during the first call the k = 0. and row = '() therefore the GI hold
; kth call: during the kth call, since the function produce-next-row work correctly, we know that it will keep on producing next row based on prev row.
;           therefore during the kth call of iter the row will be the kth row of pascal triangle.
; k+1 th call: during the k+1 call, the produce-next-row will take the kth row to produce a k+1 row of pascal triangle. During the call we see that k
;              is incremented by 1 therefore GI hold again
; Termiantion: when k = n, we know that kth call of the function, row will the kth row of the pascal triangle. so when k = n, row will the nth row.
;              of pascal triangle which is what we want to find, therefore we return the row and the function terminate.
; Problem 2a (10 points)

; We defined accumulate

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))


; Use this function to implement the function mymap so that a call (mymap p seq), where p is a procedure and seq is a
; list, returns the same value as (map p seq), where map is as defined in Scheme.

; No proof is necessary, but I do expect you to include (WORKING) test cases. 


;;;; INSERT YOUR ANSWER HERE
;(map (lambda (i) (+ i 1)) '(1 2 3))
(define (mymap p seq)
  (cond ((null? seq) '())
        (else (cons (p (car seq)) (mymap p (cdr seq))))))
;(mymap (lambda (i) (+ i 1)) '(1 2 3))
;self-----------------------------------------------------------
(define (mymap p seq)                                          ;
  (accumulate (lambda (i j) (append (list (p i)) j)) '() seq)) ;
                                                               ;
;(mymap (lambda (i) (+ i 1)) '(1 2 3))                          ;
;---------------------------------------------------------------

; Problem 2b (10 points)

; Implement a function count-leaves using accumulate and map, so that for any list t,
; (count-leaves t) returns the number of leaves of t (thinking of t as a tree).

; No proof is necessary, but I do expect you to include (WORKING) test cases.



;;;; INSERT YOUR ANSWER HERE
(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (count-leaves t)
  (cond ((atom? (car t)) (+ 1 (count-leaves (cdr t))))
        (else (+ (count-leaves (car t)) (count-leaves (cdr t))))))

;self-------------------------------------------------------------------
(define (count-leaves t)
  (cond ((null? t) 0)
        ((atom? (car t)) (+ 1 (count-leaves (cdr t))))
        (else (+ (count-leaves (car t)) (count-leaves (cdr t))))))
;----------------------------------------------------------------------
(define (turn-1 tree)
  (cond ((null? tree) '())
        ((atom? (car tree)) (append (mymap (lambda (i) 1) (list (car tree))) (turn-1 (cdr tree))))
        (else (append (list (turn-1 (car tree))) (turn-1 (cdr tree))))))
(define (flatten tree)
  (cond ((null? tree) '())
        ((atom? (car tree)) (cons (car tree) (flatten (cdr tree))))
        (else (append (flatten (car tree)) (flatten (cdr tree))))))
(define (count-leaves t)
  (let ((flat-tree (flatten (turn-1 t))))
    (accumulate + 0 flat-tree)))
;(count-leaves '(a b ( d e f)))
;-----------------------------------------------------------------------

; Problem 3a (40 points)

; As you recall, any proposition constructed with the connectives ~ (not), ^ (and), v (or) and => (implies) can be transformed
; to a logically equivalent proposition constructed using only ~ and v.  The main equivalences used are De Morgan's law, namely
; (a ^ b) = ~ (~a v ~b), and p => q = ~p v q.

; In this problem you are asked to define a datatype (with suitable representations) of propositions, and to use the resulting
; collection of constructors, selectors and classifiers to write and prove a procedure normalize which inputs a proposition using
; ~, ^, v, and => and which returns a logically equivalent proposition which uses only v and ~.

; For example, if e is the representation of ~ (a ^ b), (normalize e) will return the representation of ~a v ~b.

; As an indication of what I have in mind for the proposition datatype, e would be built as (make-not (make-and 'a 'b)).

; The procedure normalize should progress by recursive descent; your proof should be inductive -- be sure to say what you
; are inducting on.

; Be sure to include (WORKING) test cases.  



;;;; INSERT YOUR ANSWER HERE
;constructor
(define (make-not x)
  (cond ((pair? x) (append '(~) x))
        (else (list '~ x))))
(define (make-or x y)
  (cond ((pair? x) (append x '(v) y))
        ((pair? y) (append x '(v) y))
        (else (list x 'v y))))
(define (make-and x y)
  (cond ((pair? x) (append x '(^) y))
        ((pair? y) (append x '(^) y))
        (else (list x '^ y))))
(define (make-implies x y)
  (cond ((pair? x) (append x '(=>) y))
        ((pair? y) (append x '(=>) y))
        (else (list x '=> y))))
;selector
(define (get-not x) ;to get the operator not
  (car x))
(define (get-op x) ;to get other operator
  (cadr x))
(define (get-not-opr x) ;to get the not operand
  (cadr x))
(define (get-1st x) ;to get other first operand
  (car x))
(define (get-2nd x) ;to get other second oeprand
  (caddr x))
;classifier
(define (is-not x)
  (if (eq? (get-not x) '~) #t #f))
(define (is-or x)
  (if (eq? (get-op x) 'v) #t #f))
(define (is-and x)
  (if (eq? (get-op x) '^) #t #f))
(define (is-implies x)
  (if (eq? (get-op x) '=>) #t #f))

;(a ^ b) = ~ (~a v ~b), and p => q = ~p v q
(define (normalize e)
  (cond ((null? e) '())
        ((atom? e) (list e))
        ((is-not e) (make-not (normalize (get-not-opr e))))
        ((is-or e) (make-or (normalize (get-1st e)) (normalize (get-2nd e))))
        ((is-and e) (make-not (make-or (make-not (normalize (get-1st e))) (make-not (normalize (get-2nd e))))))
        ((is-implies e) (make-or (make-not (normalize (get-1st e))) (normalize (get-2nd e))))))
;(normalize '(a => b))
;(normalize '(a ^ b))

;self---------------------------------------------------------------------------------------------
;add another classifier
(define (statement? x)
  (and (not (eq? (car x) '~)) (null? (cdr x))))
;rewrite the constructor
(define (make-not x)
  (cond ((statement? x) (append '(~) x))
        (else (list '~ x))))
(define (make-or x y)
  (cond ((and (is-not x) (is-not y)) (append x '(v) y))
        ((and (is-not x) (statement? y)) (append x '(v) y))
        ((and (is-not y) (statement? x)) (append x '(v) y))
        ((is-not x) (append x '(v) (list y)))
        ((is-not y) (append (list x) '(v) y))
        ((and (statement? x) (statement? y)) (append x '(v) y))
        ((statement? x) (append x '(v) (list y)))
        ((statement? y) (append (list x) '(v) y))
        (else (list x 'v y))))
(define (make-and x y)
  (cond ((and (is-not x) (is-not y)) (append x '(^) y))
        ((and (is-not x) (statement? y)) (append x '(^) y))
        ((and (is-not y) (statement? x)) (append x '(^) y))
        ((is-not x) (append x '(^) (list y)))
        ((is-not y) (append (list x) '(^) y))
        ((and (statement? x) (statement? y)) (append x '(^) y))
        ((statement? x) (append x '(^) (list y)))
        ((statement? y) (append (list x) '(^) y))
        (else (list x '^ y))))
(define (make-implies x y)
  (cond ((and (is-not x) (is-not y)) (append x '(=>) y))
        ((and (is-not x) (statement? y)) (append x '(=>) y))
        ((and (is-not y) (statement? x)) (append x '(=>) y))
        ((is-not x) (append x '(=>) (list y)))
        ((is-not y) (append (list x) '(=>) y))
        ((and (statement? x) (statement? y)) (append x '(=>) y))
        ((statement? x) (append x '(=>) (list y)))
        ((statement? y) (append (list x) '(=>) y))
        (else (list x '=> y))))
(define (normalize e)
  (cond ((null? e) '())
        ((atom? e) (list e))
        ((is-not e) (make-not (normalize (get-not-opr e))))
        ((is-or e) (make-or (normalize (get-1st e)) (normalize (get-2nd e))))
        ((is-and e) (make-not (make-or (make-not (normalize (get-1st e))) (make-not (normalize (get-2nd e))))))
        ((is-implies e) (make-or (make-not (normalize (get-1st e))) (normalize (get-2nd e))))))
(normalize '(a => b))
(normalize '(a ^ b))
;proof:
; assume all the aux functions work
;pre: e is a proposition which can be construct by using the constructor above
; assume the input e does not violate the pre
; basis: when e is null therefore there is nothign to be nomalize, we simple return null. if e is an atom so it is a statement, so we can also simply
;        return (e)
; IH: the function will normalize the proposition e
; IS: we have cover every condition using the classifier that we have build.
;  when e is-not: if it is not we don have to chg anything but normalzie the statement of this proposition.
;  when e is-or: during the or case we don hav to chg anythign as well. we simply need to normalize the first statement and the second statement
;  when e is-and: during the and case we have to chg to the form of ~(~a v ~b), so we simply use the constructor of not and or and normalize the first
;                 statement and second statement
;  when e is-implies: similar to case and. use constructor of 'not' and 'or' and normalize the first and second statement
; Termiantion: when e is a statement, we dont have to nomalize anymore. simply return the statement and done.


; Problem 3b (10 points - Extra Credit)

; Write a function remove-double-not which works with your representation of propositions to remove double negations.  For
; example, if ~ ~ ((~ x) v (~ y)) is represented as (~ (~ ((~ x) v (~ y)))), then
; (remove-double-not '(~ (~ ((~ x) v (~ y))))) returns ((~ x) v (~ y)).

; Note that double negations may occur at arbitrary depth, so that a recursive routine is necessary.  Test your procedure
; by composing it with normalize -- what output do you get for your representation of ~ (a ^ b)?  

; You do not need to give a proof.  You do need to show that your code works on some well-chosen test cases. 





; Problem 4a.  (10 points) Write and prove correct a pair of MUTUALLY RECURSIVE programs odd-indexed-elements and
; even-indexed-elements to produce from one list L the two lists consisting, respectively, of the elements of L with
; even index and the elements of L with odd index.  For example, (odd-indexed-elements '(a b c d e f g h)) = '(b d f h)
; and (even-indexed-elements '(a b c d e f g h)) = '(a c e g).  (Hint: use one proof for both functions).  

; 4b.  (15 points) Write and prove correct a procedure mergesort which inputs a single (unsorted) list of numbers
; and which outputs the list consisting of the elements of the input list, in sorted order. 
; Your procedure should make use of your solution to 4a.

; (HINT: start by writing a procedure merge which takes two lists of numbers, with each list sorted in order from
; smallest to largest, and which produces a third list containing all the (distinct) elements of the original lists,
; in sorted order.)


; Again, for both problems, I expect you to give (working) test cases.  

(define (even-indexed-elements mylst)
  (cond ((null? mylst) '())
        ((null? (cdr mylst)) (cons (car mylst) (even-indexed-elements (cdr mylst))))
        (else (cons (car mylst) (even-indexed-elements (cddr mylst))))))
;proof:
; Basis: When the list is null, there is nothing for us to return so we return a null.
; IH: 'even-indexed-elements' will extract all the element with even index by cons the '0' index and '0' index from the rest of the list
; IS: During the call the function will check whether the list is null then check whether the list is a single element list. If it is a single element
;     list then we return the cons of '0' index element and null. Otherwise we will cons the '0' index element and cdr twice the list to 'shift' the
;     '2' index element to the '0' index and call the function again.
;termination: By keep on 'shifting' the element, eventually we will left with either null or single element in the list. If it is null we will return
;             null(basis). If it is a single element then we cons it and null. Therefore the function terminate
;(even-indexed-elements '(1 2 3 4 5))
(define (odd-indexed-elements mylst)
  (cond ((null? mylst) '())
        ((null? (cdr mylst)) '())
        (else (cons (cadr mylst) (odd-indexed-elements (cddr mylst))))))
;proof:
; Basis: same as above
; IH: 'odd-indexed-elements' will extract all the element with odd index by cons the '1' index and '1' index from the rest of the list
; IS: During the call, the function will check whether the list is null then check whether the list is a single element list. If it is a single element
;     list then we return null because that element is at '0' index which is not odd. Otherwise we will cons the '1' index element and cdr twice the
;     list to 'shift' the '3' index element to the '1' index and call the function again.
;termination: By keep on 'shifting' the element, eventually we will left with either null or single element in the list. If it is null we will return
;             null(basis). If it is a single element then we return null as well. Therefore the function terminate
;(odd-indexed-elements '(3 2 3 1))
(define (merge lst1 lst2)
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else (cond ((< (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) lst2)))
                    (else (cons (car lst2) (merge lst1 (cdr lst2))))))))
;proof:
; Basis: When either of the list is null return the other list that 'is not' null. This cover the case of when both are null. If both are null, we
;        we return null.
; IH: 'merge' will return a single list consisting all the element from both list in ascending order
; IS: Since the both list are sorted ,we will only comparing the first element from both list. if either one of them is smaller than the other we cons
;     that element and call merge with that both lists again. Except this time we will cdr one of the list which is the one that conatain the smaller
;     element. By keep on taking the smaller element from either of the list, we will 'link' all the element from both list in ascending order.
;termination: Eventually one of the list will become null if we keep on cdr-ing on either of them. Then we return the one that is not null to be cons
;             at the back.
;(merge '(1) '(2))
(define (mergesort lst)
  (cond ((null? lst) '())
        ((= 1 (length lst)) lst)
        (else (merge (mergesort (even-indexed-elements lst)) (mergesort (odd-indexed-elements lst))))))
;(mergesort '(3 3 54 2 3 4 5 7 2))
;proof:
; Basis: when the list is consisting of only one elemnt then it is considered as sorted so we simply return that list. OR the list is null we
;        will return null cause there are nothing
; IH: mergesort will input an unsorted list lst and return a list LST that are sorted.
; IS: we begin the mergesort by spliting the lst into two lst (lst1 & lst2) then merge those two. Since we know both of the function "even..." and
;     "odd..." will work correctly, eventually we will be sorting a list with one element or null. Then we merge them together. By the proof above we
;     knwo that the merge will merge any sorted two list into one sorted list.
;Termination: Eventually all the element in the list will be splitted into single elemtn list. Since we know single element list is considered as sorted
;             so we can merge those two sorted list. Since we know merge will work. Eventually all the single element list will be merge together become
;             one sorted LST.
















