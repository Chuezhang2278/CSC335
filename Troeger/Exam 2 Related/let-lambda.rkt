(define (make-addConstant x)
  (lambda (y) (+ x y)))

((make-addConstant 4)5)

(define add4 (make-addConstant 4))

(add4 3)

(define (test x)
  (let ((name1 (remainder x 3)))
    (if (= name1 1) #t #f)))

(test 11)
  
