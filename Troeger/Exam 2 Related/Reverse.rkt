(define x (list (list 1 2) (list 3 4)))

(define (last-element lis)
  (cond ((null? (cdr lis)) (car lis))
        (else (last-element (cdr lis)))))

(define (remove-last lis)
  (cond ((null? (cdr lis)) '())
        (else (cons (car lis) (remove-last (cdr lis))))))

(define (reverse-func lis)
  (cond ((null? lis) lis)
        (else (cons (last-element lis) (reverse-func (remove-last lis))))))

(define (deep lis)
  (cond ((null? lis) lis)
        ((pair? (car lis)) (cons (reverse-func (car lis)) (deep (cdr lis))))))

(define (deep-reverse lis)
  (reverse-func (deep lis)))

(deep-reverse x)

;Basis Step: If the list is empty, then return empty list

;IH: We assume that this function will work as long as the list is not empty
;
;IS: Since we are assuming that the IH is true, then if (deep-reverse x) works
;    Then (deep-reverse (deep-reverse x)) also holds true
;
;Termination : According to the code, the function will halt when the list is empty and as long
;              as the given list is not empty, it will eventually become empty.