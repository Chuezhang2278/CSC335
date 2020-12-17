(define (foldr operation start lis)
  (if (null? lis) start
      (operation (car lis) (foldr operation start (cdr lis)))))

(foldr + 0 (list 1 2 3 4 5))

(define (foldl operation total lis)
    (if (null? lis) total
        (foldl operation (operation total (car lis)) (cdr lis))))
  

(foldl + 0 (list 1 2 3 4 5))

(define lis '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (accumulate-n operation start lis)
  (if (null? (car lis)) '()
      (cons (foldr operation start (map car lis))
            (accumulate-n operation start (map cdr lis)))))



(accumulate-n + 0 lis)