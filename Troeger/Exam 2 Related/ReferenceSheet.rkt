; map
(define (mymap f l)
  (cond ((null? l) l)
	(else (cons (f (car l)) (mymap f (cdr l))))))

; prime factors, also need to add filters
; dev: What i would do is i would break this up into pieces
;      One part will be dedicated to just finding the prime factors
;      The other part will be dedicated to filtering and output
;
;      I can just use one function called prime factors to find all prime factors
;      it will have a paramter with a divisor which will incremement + 1 per function call
;      if number % divisor = 0 then it we cons it in and continue
;      if number % divisor != 0 then we incremement
;      For the filtering we'd just get the car of list, count # of time we see car list then output like (factor, length of lis) then remove from the main list we are processing

(define (PrimeFactors number)
  (define (factor divisor number)
    (display divi
    (if (> (* divisor divisor) number) 
        (list number)
        (if (= (modulo number divisor) 0)
            (cons divisor (factor divisor (/ number divisor)))
            (factor (+ divisor 1) number))))
  (factor 2 number))

; accumulate
(define (fold-right operation total lis)
  (cond ((null? lis) total)
        (else (operation (car lis) (fold-right operation total (cdr lis))))))

; accumulate left
(define (foldl operation total lis)
    (if (null? lis) total
        (foldl operation (operation total (car lis)) (cdr lis))))

; accumulate sequence
(define (accumulate-n operation start lis)
  (if (null? (car lis)) '()
      (cons (foldr operation start (map car lis))
            (accumulate-n operation start (map cdr lis)))))

;flatten
(define (flatten lis)
  (cond ((null? lis) lis)
        ((pair? lis) (append (flatten (car lis)) (flatten (cdr lis))))
        (else (list lis))))

;all but first n | Pre: n > 0, lis it not null, Length of list > n 
;                | post: list of all but the first n elements of L
(define (func lis n)
  (cond ((null? lis) lis)
        ((> n 0) (func (cdr lis) (- n 1)))
        (else lis)))

;GI: Total-work = Work-done + Work-remaining
;  : all but first n elements of L = digits in L that are processed + digits in L that are not processed


(define (mymap f l)
  (cond ((null? l) l)
	(else (cons (f (car l)) (mymap f (cdr l))))))

(define (mymap2 func lis)
  (accumulate (lambda (x y) (cons (func x) y)) '() lis))
