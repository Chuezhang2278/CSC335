(define (factorial x)
  (cond ((zero? x) 1)
        (else (* x (factorial (- x 1))))))

(define (factorial-iter x)
  (define (fact x total)
    (cond ((zero? x) total)
          (else (fact (- x 1) (* total x)))))
  (fact x 1))

(define (pascal n)
  (define (nth-row-pascal n x) ;x starts at 1 and iterates until n
    (cond ((zero? n) (list 1))
          ((= (+ n 1) x) (list 1))
          (else (cons (/ (factorial n) (* (factorial (- n (- x 1))) (factorial (- x 1))))
                      (nth-row-pascal n (+ x 1))))))
  (nth-row-pascal (- n 1) 1))


  
  

                 
                 