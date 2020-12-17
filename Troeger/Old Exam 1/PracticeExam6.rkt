;https://i.imgur.com/juJNjCb.png

;Iterative method

(define (sum-iter num curr)
  (if (zero? num) curr
      (if (= (remainder (remainder num 10) 2) 1) (sum-iter (quotient num 10) (+  curr (remainder num 10)))
          (sum-iter (quotient num 10) curr))))
       
(define (sum-odds-I num1 num2)
  (+ (sum-iter num1 0) (sum-iter num2 0)))

(sum-odds-I 123 456)

(define (sum-odds-II num1 num2)
  (define (sum-iter2 num curr)
  (if (zero? num) curr
      (if (= (remainder (remainder num 10) 2) 1) (sum-iter2 (quotient num 10) (+  curr (remainder num 10)))
          (sum-iter2 (quotient num 10) curr))))
  (+ (sum-iter2 num1 0) (sum-iter2 num2 0)))

(sum-odds-II 123 456)

;This is technically the method i was suppose to use but it does not work so i have to do method above. I have
;seen this type of error happen in different types of questions but have no solution for them.


; Guess Invariant: Check the last digit to and check if it is an Odd digit
;
; Strong enough?(exit): GI + Stop-Condition -> Post-Condition
;                     : Total = Work-So-Far + Work-Remaining + Stop-Condition
;                     : Total = Right-Most-Digit(Is-Odd(k)) + Remanining-Digits(Is-Odd(k-1)) 
;
; Weak Enough?(init) : Test to see if first loop is successful
;                    : Total = Right-Most-Digit(Is-Odd(k)) + Remaining-Digits(Is-Odd(k-1)) 
;                    : Total = Right-Most-Digit(Is-Odd(k)) + "Empty-Value"
;                    : Total = Right-Most-Digit(Is-Odd(k))
;
; Preserved?(Curr + Next) : Total = Work-So-Far + Work-Remaning
;                         : Total = Right-Most-Digit(Is-Odd(k)) + Remaining-Digits(Is-Odd(k-1))
;                         : Total = Right-Most-Digit(Is-Odd(k)) + Right-Most-Digit(Is-Odd(k-1)) + Remaining-Digits(Is-Odd(k-2))
;                         : Work-So-Far = Right-Most-Digit(Is-Odd(k)) + Right-Most-Digit(Is-Odd(k-1)) ||| Work-Remaininig = Remaining-Digits(Is-Odd(k-2))
;                         : Total = Work-So-Far + Work-Remaining


