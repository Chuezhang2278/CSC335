;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 10_6_lect) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define x 17)

(define (funs x)
  (+ x x))

(funs 1)

(define (make-add x)
  (lambda (y)(+ x y)))

(define add4 (make-add 4))

((make-add 4)1)
