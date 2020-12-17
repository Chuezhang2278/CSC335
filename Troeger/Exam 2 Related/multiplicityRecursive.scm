; Say that two non-negative integers are similar if they contain the same digits, with the
; same multiplicity. Thus 123123 and 312312 are similar, as are 110 and 101. (011 does not
; enter the discussion, because we do not permit leading 0s.) On the other hand, 1203 and 12300 are
; not similar.

; Specifications:
; Pre: a,b are non-negative integers
; Post: (a,b are similar)
; Notes: a,b are similar if they contain same digits
;        leading 0s not permitted ex. 011

; Development:
; My plan is to extract the rightmost digit of a using remainder and quotient
; and then scan and remove that digit in b,
; Sketch:
; a = 123123 and b = 312312
; (remainder a 10) = 3 | new a = (quotient 123123 10) = 12312
; (remove b 3) = (remove 312312) = 31212 | we can scan the digits of b by extracting rightmost using remainder and quotient
; If we get that a = 0 = b, then the two non-negative integers are similar
; For the case that the integers do not have the same digits,
; that is when remove fails to remove the specified digit from b,
; the program should return false

; Sketch for remove:
; (remove 312312 3)
; (remainder 312312 10) = 2 | 2 = 3?, no so check next | 2*10^0 + (remove (quotient 312312 10) 3)
; (remove 31231 3)
; (remainder 31231 10) = 1 | 1 = 3?, no so check next | 2*10^0 + 1*10^1 + (remove (quotient 31231 10) 3)
; (remove 3123 3)
; (remainder 3123 10) = 3 | 3 = 3?, yes! so we remove | 2*10^0 + 1*10^1 + (quotient 3123 10)*10^2
; 2*10^0 + 1*10^1 + (quotient 3123 10)*10^2
; 2*10^0 + 1*10^1 + 312*10^2
; 2*10^0 + 1*10^1 + 31200
; 2*10^0 + 31210
; 31212

; Looks like we need a count starting at 0 incrementing by 1 at each recursive call for the expt

; Code:
; Pre: b >= 0 is an integer, 0 <= digit < 10 is an integer, count >= 0 is an integer
(define (remove b digit count)
  (cond ((= b 0) 0)
        ((= (remainder b 10) digit)
         (* (quotient b 10) (expt 10 count)))
        (else (+ (* (remainder b 10) (expt 10 count))
                 (remove (quotient b 10) digit (+ count 1))))))
; Post: b returned with rightmost occurence of digit removed

; Testing:
(remove 312312 3 0)
(remove 312312 5 0)

; Now that we have a method of removing a digit, we can work on the main function:
; Cases to check:
; If a = b, a = 0 = b, then they are similar
; if a = 0, but b > 0, they are not similar
; if a > 0, then we need to keep checking

; Another potential problem I thought about:
; what if digits(a) > digits(b)
; suppose a = 312 and b = 3
; the program will call them similar?
; So maybe a revised test case could be
; If a = b, a = 0 = b, then they are similar
; if digits(a) != digits(b), they are not similar
; Otherwise, keep checking

; Pre: n >= 0 is an integer
(define (digits n)
  (cond ((< n 10) 1)
        (else (+ 1 (digits (quotient n 10))))))
; Post: number of digits of n returned

; Testing:
(digits 312312)

; Attempt 1:
; Pre: a, b are non-negative integers
; (define (similar? a b)
;   (cond ((and (= a b) (= a 0) (= b 0)) #t)
;         ((not (equal? (digits a) (digits b))) #f)
;         (else (similar? (quotient a 10) (remove b (remainder a 10) 0)))))
; Post: (a and b are similar) either #t or #f

; Testing:
; (similar? 123123 312312)
; (similar? 110 101)
; (similar? 1203 12300)
; (similar? 1243 1235)
; (similar? 1243 123)

; Is this recursive?
; Is similar? acting as a guard for remove ?

; Sketch:
; (similar? 123123 312312)
; (similar? 12312 (remove 312312 3 0))
; (similar? 12312 31212)

; No...
; But, what if we made an and guard
; If the anding of #t and the recursive call is true, then a and b are similar
; If the anding is #f then they aren't

; Attempt 2 at code:
; Pre: a, b are non-negative integers
(define (similar? a b)
  (cond ((or (and (zero? a) (zero? b)) (equal? a b)) #t)
        ((not (equal? (digits a) (digits b))) #f)
        (else (and #t (similar? (quotient a 10) (remove b (remainder a 10) 0))))))
; Post: (a and b are similar) either #t or #f

; Testing:
(similar? 123123 312312)
(similar? 110 101)
(similar? 1203 12300)
(similar? 1243 1235)
(similar? 1243 123)

; Sketch:
; (similar? 123123 312312)
; #t and (similar? 12312 (remove 312312 3 0))
; #t and (similar? 12312 31212)
; #t and #t and (similar? 1231 (remove 31212 2 0))
; #t and #t and (similar? 1231 3121)
; #t and #t and #t and (similar? 123 (remove 3121 1 0))
; #t and #t and #t and (similar? 123 312)
; #t and #t and #t and #t and (similar? 12 (remove 312 3 0))
; #t and #t and #t and #t and (similar? 12 12)            ; Note I have a case for a = b return #t
; #t and #t and #t and #t and #t
; #t and #t and #t and #t
; #t and #t and #t
; #t and #t
; #t

; Looks recursive to me