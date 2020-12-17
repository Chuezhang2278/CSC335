

; Second Homework Set
; CSc 335
; Fall 2020


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is a long problem set - you will want to set aside some hours.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; I am posting this on September 15, somewhat in advance of discussing
; the relevant material in lecture.  As I have said, it is a good idea
; to start thinking about the problems right away: you'll be primed to
; pick up on information which will help you solve them.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Proofs must be given for all programs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Abelson and Sussman, Problems 1.11 and 1.12

; 2.  Write iterative and recursive scheme functions to return the sum of the digits within
; a non-negative integer.  For example, (sum-of-digits 345) is 12.

(define (num x)
  (if (zero? x) x
      (+ (modulo (floor x) 10) (num(/ (floor x) 10)))))

(num 3456)


; 3.  Write iterative and recursive scheme programs to test whether the digits in a non-negative
; integer are in increasing order.  For example, the digits of 12348 are in increasing order, while
; those of 12343 are not.

(define (increaseOrder x)
  (cond ((< x 10) #t)
        ((< (modulo x 10) (modulo (quotient x 10) 10)) #f)
        (else (increaseOrder(quotient x 10)))))

(increaseOrder 231)
(modulo 0134 10)
(modulo (quotient 231 10) 10)
; You may find the built-in functions quotient, remainder, truncate, zero? -- and perhaps others --
; helpful as you design your solutions for problems 2 and 3.  Have a look at the Scheme
; manual.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
