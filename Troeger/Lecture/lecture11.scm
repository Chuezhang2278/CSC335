
; CSc 335
; Tuesday November 10 2020

; lecture11.scm




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here is another problem for you to think about -- constructing the second interpreter
; of the semester.  




; (Due to Friedman and Felleisen, the authors of TLS)

; An L-expression is an S-expression which is either:
;   (AND l1 l2), or
;   (OR l1 l2), or
;   (NOT l1), or
;   an arbitrary symbol, which we call a variable

; Here l1 and l2 are arbitrary L-expressions, so this is an inductive definition once 
; we add 'and nothing else is an L-expression'

; (a) Write and certify a function lexp? which checks whether an S-expression is an L-expression.

; (b) Write and certify a function covered? of an L-expression lexp and a list of symbols los that tests
;     whether all the variables in lexp are in los.

; (c) For the evaluation of L-expressions we need association lists, or alists.  An alist for
;     L-expressions is a list of (variable, value) pairs.  The variable component is always a symbol, and
;     the value component is either the number 0 (for false) or 1 (for true). Write and certify a function
;     lookup of the symbol var and the association list al, so that (lookup var al) returns the 
;     value of the first pair in al whose car is eq? to var.

; (d) If the list of symbols in an alist for L-expressions contains all the variables of an L-expression
;     lexp, then lexp is called _closed_ with respect to this alist.  A closed L-expression can be evaluated,
;     essentially by substituting the values of the variables given in the alist for the variable occurrences
;     in the L-expression.  You are asked to write and certify the function value of an L-expression 
;     lexp and an alist al, which, after verifying that lexp is closed with respect to al,
;     determines whether lexp means true or false.  If lexp is not closed wrt al, then (value lexp al)
;     should return the symbol not-covered.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; We discuss the solution - in outline.  The emphasis of our discussion is the simple nature of
; the variable environment: every variable is global.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For scheme, we need somewhat more complex environment structures.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; to see this, let's consider some lambda forms and their associated environments

; example 1

((lambda (a b)      
   (+ a b))
 2 3)

; the expression (+ a b) is evaluated in this environment

; (a b)----|----(2 3)


; essentially the same flat structure used for L-exps, perhaps represented as a pair of lists -- the
; first is a list of names, i.e., (a b), and the second is the list of corresponding values, (2 3)



; example 2


((lambda (a b)
   (+ a (* b ((lambda (a c) (+ (* a c) b))
	      3 4)                                                  
	   )))
 1 2)


; the expression (+ (* a c) b) is evaluated in this environment 

; (a c)---|---(3 4)
;         |
; (a b)---|---(1 2)


; and can be seen to have value (+ (* 3 4) 2) = 14.  The search for a given symbol begins with the first rib,
; proceeding to the second rib only if the symbol does not occur in the first rib.  What happens if a symbol does
; not occur in any rib?


; when this expression has been evaluated, the outer expression (+ a (* b 14)) is evaluated in


; (a b)---|---(1 2)


; to give (+ 1 (* 2 14)) = 29


; these environment structures are usually described as 'rib-cage environments': you can see the ribs, and the
; sternum, suggested in our diagrams

; clearly, any interpreter is going to need to 'push' and 'pop' ribs at appropriate times.  
 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we start off our construction of a scheme interpreter by building its environment subsystem, following the
; interpreter presented in chapter 10 of the little schemer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; environments implemented as tables, i.e., rib-cages

; pre:
; name is a symbol
; table is a rib-cage, that is, a list of entries, where each entry is a list (names values), where names
;    is a list of symbols and names is the corresponding list of values.  The lists names and values have the
;    same length, and the kth name corresponds to the kth value. 
; table-f is a function of one argument, name, which specifies the action taken when name does not occur in
;    any of the entries of the table


; post:
; returns the value associated with name in table - note that the search is entry by entry, starting with the first
; entry - if this exists, otherwise returns (table-f name)


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


; pre: name is a symbol
;      entry is a pair of lists (names vals), where names is a lat, and vals is a list of values with length at least
;            as long as that of names
;      entry-f is a function which specifies the action taken when name does not occur in names

; post: returns the value associated with name, with positional association: the kth name is associated to the kth value,
;       if name occurs in the names component of entry.  Otherwise calls entry-f with argument name. 


(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (names entry)
                          (vals entry)
                          entry-f)))



(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car vals))
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr vals)
                                  entry-f)))))

; simultaneously cdr down the lists names and vals 



; here are constructor and selectors for entries

(define build
  (lambda (s1 s2)
    (list s1 s2)))

(define new-entry build)

(define names
  (lambda (entry) (car entry)))

(define vals
  (lambda (entry) (cadr entry)))




; here are the basic definitions for tables

(define initial-table '())

(define table-f
  (lambda (name)
    (car (quote ()))))

; note that as (car (quote ())) throws an error: when the table is empty,
; attempting to look anything up in it is an error.  See the definition
; for lookup-in-table, above. 

; we could just use the error primitive, but I wanted to explain
; the presentation given in tls.  Why didn't they use error?  Perhaps
; to make the point that it is not necessary to have this primitive.
; Or perhaps because error was not included in standard scheme at
; the time the book was written.


(define extend-table cons)






; testing data

(define names1 '(a b))
(define vals1 (list 1 2))
(define entry1 (new-entry names1 vals1))

(define names2 '(a c))
(define vals2 (list 3 4))
(define entry2 (new-entry names2 vals2))

(define table1 (extend-table entry1 initial-table))
(define table2 (extend-table entry2 table1))



; a few tests

(lookup-in-table 'a table2 table-f)

; observe that this returns 3, not 2: a is found in the topmost rib, and so the second rib is never searched


(lookup-in-table 'b table2 table-f)

; b is not present in the first rib, and so the second rib is searched


; from the call

;; (lookup-in-entry name
;; 		 (car table)
;; 		 (lambda (name)
;; 		   (lookup-in-table name
;; 				    (cdr table)
;; 				    table-f)))

; we see that entry-f has been bound to (lambda (name) (lookup-in-table name (cdr table) table-f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;^^^^^^^^^^^

; and that the topmost entry is discarded for the continuation of the search.  We are cdring down the table. 



(lookup-in-table 'd table2 table-f)

; as expected, this fails




; Perhaps one additional remark is in order for this first look at the environment subsystem of the tls scheme
; interpreter: observe that ANY scheme value at all can occur in the values lists.  We used integer values for
; our example, but soon we will take advantage of the genericity of scheme's lists by (for example) having function
; descriptions occur in them


(define names3 (list 'e 'id))
(define vals3 (list 17 '(lambda (x) x))) ; (lambda (x) x) is here regarded as a description of the identity function
(define entry3 (new-entry names3 vals3))
(define table3 (extend-table entry3 table2))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now we begin discussion of the entire scheme interpreter presented in Chapter 10 of The Little Schemer.  This interpreter,
; which we shall refer to as TLS Scheme, or just TLS, implements a subset of Scheme, which we shall also refer to
; as TLS.  Whether we mean the interpreter or the language will be clear from context.


; TLS scheme has just 6 types of expressions, named *const, *quote, *identifier, *lambda, *cond, and *application

; More precisely, the syntax of TLS can be given inductively as follows


; basis step of an inductive definition of TLS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; numbers, #t, #f, cons, car, cdr, null?, eq?, atom?, zero?, add1, sub1, number? have type *const -- we may find it
; desirable to add others later  -- as you will see, this is very easy to do

; symbols have type *identifier




; inductive step of an inductive definition of TLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (quote x) has type *quote, where x is any TLS expression

; (lambda varlist lambda-body) has type *lambda, where varlist is a list of *identifiers and lambda-body is any TLS expression

; (cond (p1 e1) ... (pk ek)) has type *cond, where the pi and ei are any TLS expressions

; (e1 e2 ... en) has type *application, where the ei are any TLS expressions




; Of course, syntax says nothing at all about the meaning of expressions - it is the interpreter which supplies
; meaning for expressions in TLS.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






; It will be helpful to look at TLS (the interpreter) in layers.  Let us begin by examining the evaluation of
; expressions without variables.  You may find the complete interpreter posted on Piazza.



; If the top level of the interpreter is given as

(define value
  (lambda (e)
    (meaning e (quote () ))))


(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


; then we can say that the interpreter computes the value of a given TLS expression e by calling the meaning function
; with the empty list as table parameter.  Here the system will use tables - as sketched above - to organize
; information about variables' values.  Our limitation to variable-free expressions for this initial look allows us to
; assume that table always has the value (quote ()).  


; from the definition of the meaning function, we are motivated to look into expression-to-action


(define expression-to-action
  (lambda (e)
    (cond 
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))


; where

(define (atom? e)
  (and (not (null? e)) (not (pair? e))))


; now one thing at a time, in an attempt to manage complexity -- we look at atom-to-action


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


; where

(define *const
  (lambda (e table)
    (cond 
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

; with

(define build
  (lambda (s1 s2)
    (list s1 s2)))

; and

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))




; as we are assuming there are no identifiers, for the moment, we say nothing further at this time regarding *identifier.



; We can already evaluate some simple expressions with the code we have:


(value '1)


; returns 1  -- work it out!!!


(value '#t)

; returns #t



(value 'sub1)

; returns (primitive sub1)

; here the tag 'primitive will be used later to further steer the evalution of expressions involving sub1.



; note the need to quote the argument to value: we want to block mzscheme (drracket) from evaluating the argument to
; value -- the argument to value is supposed to be a TLS expression which is evaluated by our TLS interpreter, and we
; block mzscheme from evaluating it by using quote.




















