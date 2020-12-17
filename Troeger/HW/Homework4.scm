
; Fourth Homework Set
; CSc 335
; Fall 2020


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework4.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here are some homework problems to get you started with lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that I have sometimes deliberately offered incomplete specifications - if you find this
; to be the case, you will need to complete the specification as you deem best.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Give both recursive and iterative procedures (along with their arguments) for each

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Write your own version of length using the list functions we have discussed.  You can find
; length documented at http://www.schemers.org/Documents/Standards/R5RS/

; The idea is suggested by (my-length '(a b c d)) = 4.
(define x (list 1 3 5 7 9))

(define (lengths lst)
  (if (null? lst) 0
      (+ (lengths (cdr lst)) 1)))

(lengths (list 1 2 3 4 5 9))  

; 2.  Write your own version of list-ref using the list functions we have discussed.  You can find
; list-ref documented at http://www.schemers.org/Documents/Standards/R5RS/
; Briefly, the idea is indicated by this example:  (my-list-ref '(a b c d) 2) = c.  Note the 0-based
; indexing.  What happens if the input index exceeds the size of the input list?

(define (searching lst index)
  (cond ((null? lst) 0)
        ((zero? index) (car lst))
        (else (searching (cdr lst) (- index 1)))))

(searching x 3)

; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.

(define (first-n lst num)
  (cond ((and (not(null? lst)) (not (zero? num))) (cons (car lst) (first-n (cdr lst) (- num 1))))))

(first-n x 4)


; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst.

(define (last-n lst num)
  (first-n lst (- (lengths lst) num)))

(last-n x 2)

; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.

(define (one? num)
  (if (= num 1) #t #f))

(define (last-ele lst num)
  (cond ((null? lst) 0)
        ((zero? num) (cdr lst))
        (else (last-ele (cdr lst) (- num 1)))))

(last-ele x 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; suggested reading:

;;    http://en.wikipedia.org/wiki/Scheme_(programming_language)

