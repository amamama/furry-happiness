(define f (lambda args (cons (car args) (cdr (cdr args)))))
(define g (lambda (head1 head2 . tail) (cons (_add head1 head2) tail)))
(cons (f 1 2 3 4) (g 1 2 3 4))
