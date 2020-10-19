(define a 1)
(define b 2)
(set! a (cons a b))
(set-car! a 5)
(set-cdr! a 3)
a
