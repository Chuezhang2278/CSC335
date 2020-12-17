; CSc 335
; Fall 2019

; October 17

; First Midterm Exam - 1.5 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 
; (I will email your graded paper to this address - DO NOT use a gmail address, as gmail seems to block name.scm 
;  files.  Please use your citymail address.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - final code (out of 20 points)  5
;;;; Problem 1 - development (out of 40 points)  5     -- you write a recursive sort, when iterative was specified

;;;; Problem 2 - final code (out of 15 points)  5
;;;; Problem 2 - development (out of 25 points)  5




;;;; Total   20
;;;; Letter Grade D-

;;;; my comments are behind ;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the lectures and homework:

; no lists, no vectors, no strings, no assignment...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones (and tablets, etc) are to be switched off and placed in your backpack/briefcase.  They are not to leave the room, nor are
; they to be visible at any point during the exam.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description of General Problem

; Say that two non-negative integers are similar if they contain the same digits, with the
; same multiplicity.  Thus 123123 and 312312 are similar, as are 110 and 101.  (011 does not
; enter the discussion, because we do not permit leading 0s.)  On the other hand, 1203 and 12300 are
; not similar.

; In this exam, you are asked to give complete developments of two distinct solutions to the problem of determining whether two
; non-negative integers are similar. 

; As in class, a complete development is one which shows all of the guess-invariants and guess-induction-hypotheses and guess-designs and guess-programs,
; along with the reasons (arguments) for changing these as you make progress, and which culminates in a working program.
; I want to see a record of your development process.  If the correctness of your program is not evident from your development,
; you will need to give a separate proof.  

; I also need to see working tests for each of your functions. 


; Problem 1 (60 points) One way to solve the problem is to implement a function similar? which inputs non-negative integers m and n, sorts
; the digits of these numbers using a function sort, and then compares (sort m) and (sort n) for equality.  You need a sort routine which
; preserves multiplicities of digits -- for example, (sort 401030) should return 431000.  I suggest using insertion sort, but you may use
; any sorting algorithm.   Use only tail recursion (iteration).    

; INSERT YOUR ANSWER HERE
;; My work

;; So for the first step we have to sort the number from greatest to smallest order by using the function sort

;; The sorting algorithm i will use in this case is insertion sort. My idea is to take the last digit as the current digit
;; and then compare to the first element of a sorted number, if current digit is < the first element of the sorted number, then we would insert to the front
;; else we would insert to the back of the element. The will form a recursion call and eventually the code will return the correct sorted number

;; Since we are doing iteration, we would have to count the length of the number first
;;;; I try hard to come up with a iterative solution, but i fail to do so. Therefore i only have recursion for now
(define (number-length num)
  (define (len-iter number count)
    (cond ((= number 0) count)
          (else (len-iter (quotient number 10) (+ count 1)))))
  (len-iter num 0))
;;;; I try hard to come up with a iterative solution, but i fail to do so. Therefore i only have recursion for now


;;;; This procedure tends to make a recursive call such as it will insert the last index of the number into a sorted-number
;;;; I have to multiply the inserted result by 10 each time because it has to make sure that it doesn't go to the wrong index
;; This is my sorting function, which takes the current index and insert it to an already sorted sequenc of number
(define (insert current-index sorted-number)
  (cond ((< sorted-number 10)
         (cond ((< current-index sorted-number) (+ (* sorted-number 10) current-index))
               (else (+ (* current-index 10) sorted-number))))
        (else (
               let ((sorted-digit-quotient (quotient sorted-number 10))
                    (sorted-digit-remainder (remainder sorted-number 10)))
                    (cond ((< sorted-digit-quotient current-index) (+ (* (sort (insert current-index sorted-digit-quotient)) 10) sorted-digit-remainder))
                          (else (+ (* (sort (insert sorted-digit-quotient current-index)) 10) sorted-digit-remainder)))))))

;; This is my sort function, which sorts a number from greatest to smallest
(define (sort number)
  (cond ((= (quotient number 10) 0) number)
        (else (
               let ((current-digit (remainder number 10))
                     (sorted-number (quotient number 10)))
                 (insert current-digit (sort sorted-number))))))

;;(define (sort2 number)
;;  (define len (number-length number))
;;  (define (sort2-iter result count)
;;    (cond ((= count len) result)
;;        (else (
;;               let ((current-digit (remainder number 10))
;;                     (sorted-number (quotient number 10)))
;;                 (sort2-iter (insert current-digit (sort2-iter sorted-number (+ count 1))) (+ count 1))))))
;;  (sort2-iter 0 0))

(sort 123)
(sort 1234)
(sort 321)

(define (similar? num1 num2)
  (cond ((= (sort num1) (sort num2)) #t)
        (else #f)))

;;;; notice that the cond adds nothing at all here, since = is already a boolean valued function

(similar? 12 21)
(similar? 123 321)
(similar? 312 312)


;;;; yes, but (similar? 30401 10034) returns #f.

;;;; the problem is with sort -- for example, (sort 10034) returns 43010, which is not right


              
; Problem 2 (40 points)  Another way to solve the problem is to implement a function alt-similar? which does not sort the digits of the two
; inputs, m and n.  Do this.  Use only proper recursion.  

; INSERT YOUR ANSWER HERE

; My idea is to make a procedure which count the sum of all digits in the number. Since the two numbers are similar if their digits are the same,
; So we know that if the sum of num1 and sum of num2 are equal, then it is similar

;;;; NO!!! 40 and 22 have the same digit sums, but they are not similar


(define (sum num)
  (define last (remainder num 10))
  (define next (quotient num 10))
  (cond ((= num 0) 0)
        (else (+ last (sum next)))))

(define (alt-similar? num1 num2)
  (cond ((= (sum num1) (sum num2)) #t)
        (else #f)))


(alt-similar? 1230 3210)
(alt-similar? 132 123)
(alt-similar? 431 134)
(alt-similar? 431 034)

; I will be inducting function sum first

; Precondition: the element must be >= 0
; BS: When num is 0, the response should return 0. Which is clearly true
; IH: for 0 < i < k, we assume that (sum i) will return the sum of all the digits
; IS: on the kth call, which returns (+ last (sum (- k 1))). By our IH, we know (- k 1) is clearly < k. Therefore
; by the definition of sum, this clearly holds
; Termination argument: Since num will be divide 10 in each iteration. Therefore it will eventually go to 0. Then will terminate

 


