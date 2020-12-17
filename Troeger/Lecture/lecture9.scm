
; CSc 335
; October 27 2020

; lecture9.scm



; We note that the structures of the programs and proofs given so far are all very 
; similar.  What would happen, however, if the first agument were not a simple 
; list of atoms?  Suppose, for example, that we wanted to replace all occurrences 
; of old in an arbitrary (nested) list -- if the car of this list is not an atom, 
; we will have to search it for occurrences of old.  How can this be arranged?  

; More, it seems that length-based induction runs into problems when we consider
; programs inputting arbitrary lists: it is easy to produce examples where the length of one
; of the list elements exceeds that of the entire list:

; (a (b c d e) f) has length 3, while (cadr '(a (b c d e) f)) has length 4

; so the fact that myprog works correctly on all inputs of length less than 3 does not
; help when it comes to showing that myprog correctly processes (b c d e), the car 
; of the cdr of (a (b c d e) f)


; if recursion and induction cannot be based on the length of the input, we 
; need to use some other measure of size.  

; we will look next at an example of how one can carry out recursion 
; (and induction) on trees.  We are, in fact, working up to the notion of 'structural
; induction'.  


; we begin looking at tree recursion by developing the function subst*:
; its job is to replace all occurrences of the 
; atom old by new in lst, where lst is an arbitary list (that is, an s-exp).  

; a quick design idea: call subst* on (car lst), then on (cdr lst), and then somehow glue these together. 



; it will be helpful to sometimes think  of sexps as trees.

; we will settle on the tree representation suggested below, but note that others may be possible.  


; list                                tree

;  a                                    a

;  ()                                   .

;  (a)                                  .
;                                       |
;                                       a


; (a b)                                 .
;                                      / \
;                                     a   b


; (a (b c))                             .
;                                      / \
;                                     a   .
;                                        / \
;                                       b   c

; (cdr '(a (b c))) =                    .
;      ((b c))                           \
;                                         .
;                                        / \
;                                       b   c


; (car '((a b) (c d e)))                 .
;                                       / \
;                                      a   b


; (cdr '((a b) (c d e)))                 .
;                                         \
;                                          .
;                                        / | \
;                                       c  d  e


; (cons (car '((a b) (c d e))) 
;       (cdr '((a b) (c d e))))           .
;                                      /    \
;                                     .      .
;                                    / \   / | \
;                                   a   b c  d  e


; and so on: to every sexp, there corresponds one and only one tree (under the correspondence
; we are discussing)

; because we can also go the other way - from trees unambiguously to lists - and because this
; correspondence respects car, cdr and cons - we can speak of sexps as trees, and trees as sexps.

; [we briefly mention the notion of 'commutative diagrams']

; This correspondence between lists and trees 
; allows us to think clearly about recursion on such structures. If we 
; agree that one tree s is 'smaller' (or simpler) than another tree t if s has fewer nodes
; than t, then we can now see that both (car s) and (cdr s) are simpler than s.

; Alternately, if we agree that
; one tree s is simpler than another tree t if s is shorter than t, or if s is no 
; taller than t but narrower than t, then it is clear that (car s) is simpler than s,
; and also that (cdr s) is simpler than s.  

; (Note that this alternative definition of 'simpler' really is different from the first:
; for example, consider (a b c d) vs. ((a)) under both rankings. }


; so the expected induction hypothesis for some function f defined on sexps, that is, on trees, would
; give us that both (f (car s)) and (f (cdr s)) work correctly in the context of some call (f s), if s
; is an sexp. 



; let's return to subst*


; we stipulate that lst is an element of the set (sexp) described by 
;  lst ::= () | (cons atom lst) | (cons lst lst)

; and that old is an atom

; without worrying whether new is an atom, we have a specification

;  pre:   lst is an element of the set sexp 
;         old is an atom
;         new is an s-expression

;  post: the returned list, viewed as a tree, should be
;  identical to the input tree, except that all
;  occurrences of old are replaced by new

(define atom?
  (lambda (x)
    (and (not (null? x)) 
	 (not (pair? x)))))

(define subst* 
  (lambda (lst old new)
    (cond ((null? lst) lst)
          ((atom? (car lst))
           (cond ((eq? (car lst) old)
                  (cons new (subst* (cdr lst) old new)))
                 (else (cons (car lst) (subst* (cdr lst) old new)))))
          (else 
           (cons (subst* (car lst) old new)
                 (subst* (cdr lst) old new))))))

; some test data

(define lst1 '(a b c d a))

(define lst2 (list lst1 (list lst1 lst1)))

; test computations

(subst* lst2 'c lst1)

(subst* lst2 'a lst1)



; sketch of proof of subst*

; by tree induction on lst

; basis: lst is null --- let me leave this to you. 

; induction hypothesis:  assume the recursive calls all work correctly (and note carefully
; that they are all on smaller trees, as per our discussion above)

; induction step:  either (car lst) is an atom or not.  If it is an atom, we
; need to check whether it is eq? to old.  If (eq? (car lst) old), the value to
; be returned is (cons new (subst* (cdr lst) old new).  Do you see why?  Because
; (cdr lst) is an sexp (lst was an sexp, so its cdr is as well), and because 
; (cdr lst) is simpler than lst, we know from the induction hypothesis that 
; (subst* (cdr lst) old new) returns a tree identical to (cdr lst) except for
; having replaced all occurrences of old by new. When we cons new into this tree,
; we get a tree identical to the input tree, except that all occurrences of old
; have been replaced by new. 

; if (car lst) is an atom, but not (eq? (car lst) old) ...  I leave it to you 
; to argue that the code does what it needs to do.

; if, finally, (car lst) is not an atom, we need to replace all occurrences of old
; in (car lst) via (subst* (car lst) old new) and also to replace all occurrences of old
; in (cdr lst) via (subst* (cdr lst) old new).   Why does consing these together 
; give the right result?  I refer you to our tree diagrams. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 


; we next develop a predicate which determines
; whether two s-expressions have the same 'shape'

; l1 and l2 have the same shape if both are null, or
; both are atoms (here: symbols or integers) which 
; are eq?, or if (car l1) and (car l2) have the 
; same shape and (cdr l1) and (cdr l2) have the same
; shape


(define same-shape?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
      ((or (not (pair? l1)) (not (pair? l2))) #f)
      (else (and (same-shape? (car l1) (car l2))
                 (same-shape? (cdr l1) (cdr l2)))))))

; Here we have used

;   (eq? x y) = (eq? y x)


; An alternate design, given that
;   '() is not a pair

; and
;   (eq? () x) if and only if x = ()

; is

(define same-shape?
  (lambda (l1 l2)
    (cond ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
          ((or (not (pair? l1)) (not (pair? l2))) #f)
          (else
           ; both l1 and l2 are pairs
           (and (same-shape? (car l1) (car l2))
                (same-shape? (cdr l1) (cdr l2)))))))

; even simpler

(define same-shape? 
  (lambda (l1 l2)
    (cond 
      ((or (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
      (else
       ; both l1 and l2 are pairs
       (and (same-shape? (car l1) (car l2))
            (same-shape? (cdr l1) (cdr l2)))))))


; the argument for this last version would appear to be quite
; easy: either l1 and l2 are both pairs, or not.  If at least
; one of them is not a pair, then they must be eq? to have
; the same shape; otherwise, the requirement is that their
; respective cars and cdrs have the same shape.  So we can
; base the certification on a tree induction.  

; What is the form of the induction?  As there are two arguments l1 and l2, we
; need to induct simultaneously on the complexity of l1 and l2.
; An appropriate first guess for the induction hypothesis has the form "assume that
; (same-shape x y) returns the correct result for all x simpler
; than l1 and y simpler than l2".

; You should (of course) complete the argument

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we present a third example of tree recursion -- the simple interpreter for
; arithmetic expressions given in tls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; we consider the class i-aexp of fully parenthesized
; infix arithmetic expressions, defined inductively as the least class 
; containing the non-negative integers and closed under
; +, * and ^:

; if e1 and e2 are i-aexps, then so are (e1 + e2), (e1 * e2)
; and (e1 ^ e2)

; some examples of i-aexps : 1, 18, (1 + 18), ((1 + 18) * 18) 
; ( ((1 + 18) * 18) ^ (26 * 2) )


; we can of course also give a grammar for i-aexp:

; i-aexp ::= non-negative-integer | (i-aexp + i-aexp)  |
;          (i-aexp * i-aexp) | (i-aexp ^ i-aexp)


; Constructors for fully parenthesized infix expressions 
; are easily given.  The assumption is that e1 and e2 are themselves fully
; parenthesized infix arithmetic expressions (fpiae), so these constructors
; may be applied ('inductively') to such expressions to build more
; complicated expressions

(define make-+
  (lambda (e1 e2)
    (list e1 '+ e2)))

(define make-*
  (lambda (e1 e2)
    (list e1 '* e2)))

(define make-^
  (lambda (e1 e2)
    (list e1 '^ e2)))


; check that the returned value in each case is a fpiae!


; we create some i-aexps using these constructors

(define test-exp-1
  (make-+
   (make-* 4 5)
   (make-^ 2 3)))

; so now test-exp-1 is ((4 * 5) + (2 ^ 3))


(define test-exp-2
  (make-*
   (make-^ 2 3)
   (make-+ 4 5)))


; so test-exp-2 is ((2 ^ 3) * (4 + 5))



; selectors for fully parenthesized infix arithmetic expressions 
; can be defined as follows:

(define operator
  (lambda (i-aexp) 
    (car (cdr i-aexp))))

(define first-operand
  (lambda (i-aexp)
    (car i-aexp)))

(define second-operand
  (lambda (i-aexp)
    (car (cdr (cdr i-aexp)))))



; examples -- in class


; classifiers for i-aexps


(define plus-aexp?
  (lambda (aexp)
    (eq? (operator aexp) '+)))

(define times-aexp?
  (lambda (aexp)
    (eq? (operator aexp) '*)))

(define power-aexp?
  (lambda (aexp)
    (eq? (operator aexp) '^)))



; [this is a common pattern for new data types: define constructors,
; selectors and then classifiers]


; note that we might have other sorts of arithmetic
; expressions -- for example, with operators in prefix
; position (p-aexp), or with some relaxation of the requirement
; for full parenthesization (perhaps nfp-aexp)

; let's consider these as all belonging to the class 
; aexp of arithmetic expressions: each aexp is either
; a number, or it has an operator, and a first operand, 
; and a second operand

; the following procedure requires that we have defined 
; selectors and classifiers appropriate for its argument

; it also requires that procedures plus, times and expon
; have been defined before it can be used.  we give these
; definitions further on

; data abstraction at work!
					;
; inputs an aexp and returns the value of the aexp

(define value
  (lambda (aexp)
    (cond ((natnum? aexp) aexp)
          (else (cond ((plus-aexp? aexp) (plus (value (first-operand aexp))
                                               (value (second-operand aexp))))
                      ((times-aexp? aexp) (times (value (first-operand aexp))
                                                 (value (second-operand aexp))))
                      ((power-aexp? aexp) (expon (value (first-operand aexp))
                                                 (value (second-operand aexp))))
                      )))))


; natnum? might be:

(define natnum?
  (lambda (x)
    (and
     (integer? x)
     (>= x 0))
    ))


; here integer? and >= are pre-defined in scheme


; one possible implementation of plus, times and expon

(define plus 
  (lambda (m n)
    (cond ((= n 0) m)
          (else (add1 (plus m (sub1 n)))))))

(define times 
  (lambda (m n)
    (cond ((= n 0) 0)
          (else (plus m (times m (sub1 n)))))))

(define expon
  (lambda (base exponent)
    (cond ((zero? exponent) 1)
          (else (times base (expon base (sub1 exponent)))))))

; where

(define add1
  (lambda (m) (+ m 1)))

(define sub1
  (lambda (m) (- m 1)))



; one now readily computes (value test-exp-1) and (value test-exp-2)

; [it becomes quickly clear, however, that expon is extremely slow.  can you
; find a way to improve its performance?]

; can you work up a proof for value?   how is the definition of
; aexp reflected in the definition of the value function?  how do you
; suppose this is reflected in the structure of the proof of the value
; function?

; [if you begin to suspect that the proof nearly repeats the expression
; of the function, you are on to something big.]  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we apply higher order functions in the context of hierarchical data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; recall the function rember:


(define rember
  (lambda (a lat)
    (cond ((null? lat) lat)
          ((eq? a (car lat)) (cdr lat))
          (else (cons (car lat) (rember a (cdr lat)))))))


; recall the function same-shape?

(define same-shape? 
  (lambda (l1 l2)
    (cond 
      ((or (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
      (else
       ; both l1 and l2 are pairs
       (and (same-shape? (car l1) (car l2))
            (same-shape? (cdr l1) (cdr l2)))))))




; we can build a function rember-shape similar to rember, in which 
; same-shape? replaces eq?:

(define rember-shape
  (lambda (a l)
    (cond ((null? l) l)
          ((same-shape? a (car l)) (cdr l))
          (else (cons (car l) (rember-shape (cdr l)))))))


; one can imagine using other binary predicates -- why not
; make the comparison test a parameter?


(define rember-f
  (lambda (test? a l)
    (cond ((null? l) l)
          ((test? a (car l)) (cdr l))
          (else (cons (car l) (rember-f test? a (cdr l)))))))


; now one can write (rember-f same-shape? '((a)) '(a b '((a)) c d))
; as well as (rember-f eq? 'a '(b c a d e))

; alternately, we could write

(define rember
  (lambda (a l)
    (rember-f eq? a l)))

; and

(define rember-shape
  (lambda (a l)
    (rember-f same-shape? a l)))

; we are passing the functions same-shape? and eq? as parameters


; one could also rewrite rember-f as follows, where we have
; curried the function

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) l)
            ((test? a (car l)) (cdr l))
            (else (cons (car l) 
                        ((rember-f test?) a (cdr l))))))))


; now we can write

(define rember-eq
  (rember-f eq?))


; so that (rember-eq 'a '(b c a d e)) = (b c d e)

; alternatively, ((rember-f eq?) 'a '(b c a d e))



(define rember-shape
  (rember-f same-shape?))

; so that (rember-shape '((a b (c (d)))) '(a b ((a b (c (d)))) c d) = (a b c d)

; now (rember-f eq?) is the function

(lambda (a l)
  (cond ((null? l) l)
        ((eq? a (car l)) (cdr l))
        (else (cons (car l) 
                    ((rember-f eq?) a (cdr l))))))

; where the internal rember-f is the recursive call, and test? 
; has been replaced everywhere by eq?.

; emphasis: (rember-f eq?) returns a _function_: (lambda (a l) ... ) as just shown



; check your understanding: what is the meaning of 

((rember-f (lambda (x y) (> x y))) 10 '(1 20 2 3))

((rember-f >) 10 '(1 20 2 3))

((rember-f =) 10 '(1 20 2 3))


;;;;;;;;;;;;;;;;;;;;;;;

; first class functions increase expressive power

;;;;;;;;;;;;;;;;;;;;;;;;


; example -- 

; consider, for plus, times and expon as defined above

(define atom-to-function
  (lambda (x)
    (cond 
      ((eq? x '+) plus)
      ((eq? x '*) times)
      (else expon))))

; we use atom-to-function to simplify the values function given earlier from

(define value
  (lambda (aexp)
    (cond ((atom? aexp) aexp)
          (else (cond ((plus-aexp? aexp) (plus (value (first-operand aexp))
                                              (value (second-operand aexp))))
                      ((times-aexp? aexp) (times (value (first-operand aexp))
                                                (value (second-operand aexp))))
                      ((power-aexp? aexp) (expon (value (first-operand aexp))
                                                (value (second-operand aexp))))
                      )))))


; to

(define value
  (lambda (aexp)
    (cond ((atom? aexp) aexp)
          (else ((atom-to-function (operator aexp))
                 (value (first-operand aexp))
                 (value (second-operand aexp)))))))





; you should also experiment with this alternative

(define atom-to-function
  (lambda (x)
    (cond 
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else expt))))


; can you say what is going on?  





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework 5 is assigned.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
