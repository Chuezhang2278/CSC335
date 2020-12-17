;(1 2 3 4 5 6) -> (2 3 4 5 6 1)
;What i can do is take the car of the lis then put it at the back of the lis
;I will make a function that takes the car of a lis and the cdr of the lis then return when car of lis is put at the end of lis
;Function lis1 lis2 --> Function (1) (2 3 4 5 6) --> if lis2 not null then cons into lis, if null? lis then lis 1
;This will also cover atoms
;
; Proof for (max-digit n):
; Precondition: n is an integer >= 0
; Postcondition: returns the largest digit in the integer n
; Inducting on the number of digits in n, k
; Basis: If we call the function on the smallest input 0, it will return 0
; IH: Assume that the call works provided that the precondition is satisfied for k-1 digits scanned, meaning for the number 54321, we scanned the left 4
; IS: Then on our last call, k, we will find that the k-1th call will have 4 as the biggest digit, on the k-th call, it will return 5, which is bigger than 4
; Termination: Eventually it will terminate after checking the last digit in which n < 10

;Pre-condition: the list is not empty
;Post-condition: Returns the car lis
;Inducting on the elements in lis
;
;Basis step: Case 1: IF what is inputed is an empty lis, return an empty lis
;            Case 2: IF cdr lis is null then return right-most-element, n
;
;IH : We assume that the current call works such that the pre-condition holds
;     true, that if the list is not empty then we will continue to cons into our
;     final list that is to be returned
;
;IS:  On the final call when lis is empty
;     If the list is empty then cons n into the final list
;     This is under the assumption that the IH holds true that since if the previous calls
;     Pre-condition held true then it will come to an eventual halt which that the list is empty
;     which returns n.
(define (ShiftLeft lis)
  (define (Shift to-left lis)
    (cond ((null? lis) (list to-left))
          (else (cons (car lis) (Shift to-left (cdr lis))))))
  (if (null? lis) '()
      (Shift (car lis) (cdr lis))))

(ShiftLeft '(1 2 3 4 5 6))

;(1 2 3 4 5 6) -> (6 1 2 3 4 5)
;How i will do this is i will take lis and cons (car lis) until last element is reached
;I will also need a way to get the last element

;Proof for (get-list lis)
;Pre-condition: (cdr lis) is not null
;Post-condition: the car of the lis
;I will be inducting on the elements of list, n
;BS: If the cdr of the list is null, then return an empty lis
;IH: Assuming that the Pre-condition is satisfied then the n+1th call will also hold true and cons the car of the current list
;IS: On the final call, k, we will notice that the k-1th call, the list will still have 2 elements left in the list
;    and on the kth call there will be only one more element left in the list in which instead of that last element being returned, nothing is returned and program
;    Comes to a halt without cons'ing it into the list
;Termination: As long as the pre-condition holds, then eventually the function will come to a halt such that for every call
;             that satisfies the pre-codontion, the cdr of the list is processed next

;Proof for (get-last lis)
;Pre-condition: The list is not empty
;Post-condition: Returns cdr of the list
;I will be inducting on the elements of the list, n
;BS: If the cdr of the list is empty, then return lis
;IH: We assume that for every call, the pre-condition is satisfied such that if the nth call works, then the n+1th call works as well
;IS: On the final call, k, the final element of lis will be returned and only 

(define (ShiftRight lis)
  (define (get-list lis)
    (cond ((null? (cdr lis)) '())
          (else (cons (car lis) (get-list (cdr lis))))))
  (define (get-last lis)
    (cond ((null? (cdr lis)) lis)
          (else (get-last (cdr lis)))))
  (append (get-last lis) (get-list lis)))

(ShiftRight '(1 2 3 4 5 6))

;Insert to specific point in a list
(define (insert-right lis element position)
  (cond ((null? lis) lis)
        ((< 0 position) (cons (car lis) (insert-right (cdr lis) element (- position 1))))
        ((= 0 position) (cons element (insert-right lis element (- position 1))))
        (else lis)))

(define (insert-left lis element position)
  (define (insert lis element position)
    (cond ((null? lis) lis)
          ((< 0 position) (cons (car lis) (insert (cdr lis) element (- position 1))))
          ((= 0 position) (cons element (insert lis element (- position 1))))
          (else lis)))
  (insert lis element (- position 1)))

(insert-left '(1 2 3 4) 5 2)