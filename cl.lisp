(define m
 	(lambda (n) (lambda (x) (set! n x))))
(define c (m 3))
(cons (c 3) '())
