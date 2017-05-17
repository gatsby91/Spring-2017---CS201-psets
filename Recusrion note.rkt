#lang racket

; Recursion Notes

; First-digit procedure

(define first-digit
  (lambda (n)
    (if (< n 100) (quotient n 10)
        (first-digit (quotient n 10)))))

; length of a list: my worry that 0 is always returned is misguided because the procedure returns the SECOND argument
; notice the +1 for the last element of the list is added first!

(define own-length
  (lambda (lst)
    (if (null? lst) 0
        (+ 1 (own-length (rest(lst)))))))

; append two lists
; suppose I'm adding '(1 2 3) and '(4 5).  3 as the last member of the list will actually be added first!

;(cons (1) (own-append (2,3) (4,5))
 ;          (cons (2) (own-append (
                      

(define own-append
  (lambda (lst1 lst2)
    (if (empty? lst1) lst2
        (cons (first lst1) (own-append (rest lst1) lst2)))))

; performing an operation on each member of the list
(define (own-map p lst)
  (cond
    [(empty? lst) lst]
    [else (cons (p (first lst))
                   (own-map p (rest lst)))]))

(define (own-reverse lst)
  (if (empty? lst) lst
      (append (own-reverse (rest lst)) (list (first lst)))))


 
