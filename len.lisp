((lambda ()
  (define len (lambda (l) (if (eq l '()) 0 (_add 1 (len (cdr l))))))
  (define x 1)
  (set! x '(2 3))
  (len (cons x '()))
))
