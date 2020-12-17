; THERE IS A PROBLEM WITH THE WAY I SPLIT THE LISTS, NEED TO FIX

(define x '(8 1 12 2 5 6 4 1 2 1))

(define small (+ (quotient (length x) 2) 1))
(define large (- (length x)  (+ (quotient (length x) 2) 1)))

(define (left lis small_size)
  (define (left-process lis count-up-from small_size) ;Left is smaller 
    (cond ((null? lis) lis)
          ((= small_size 2) (list (car lis))) ;problem is that when the len(lis) = 2, above algorithm
          ((= count-up-from small_size) '())
          (else (< count-up-from small_size) (cons (car lis) (left-process (cdr lis) (+ count-up-from 1) small_size)))))
  (left-process lis 0 small_size))

(define (right lis large_size)
  (define (right-process lis count large_size)
    (cond ((null? lis) lis)
          ((or (> count large_size)) (cons (car lis) (right-process (cdr lis) (+ count 1) large_size)))
          (else (right-process (cdr lis) (+ count 1) large_size))))
  (right-process lis 0 large_size))

(define (merge left_list right_list)
  (cond ((null? left_list) right_list)
        ((null? right_list) left_list)
        ((< (car Left_list) (car Right_list)) (cons (car left_list)(merge (cdr left_list) right_list)))
        (else (cons (car right_list) (merge (cdr right_list) left_list)))))

(define (split lis)
  (define small (+ (quotient (length lis) 2) 1))
  (define large (- (length lis)  (+ (quotient (length lis) 2) 1)))
  (cons (left lis small) (cons (right lis large) '())))

(define (mergesort lis)
	(cond ((null? lis) lis)
              ((null? (cdr lis)) lis)
              (else (merge (mergesort (car (split lis))) (mergesort (cadr (split lis)))))))
(mergesort x)

  
