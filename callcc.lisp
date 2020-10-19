(define a 1)
(define b 2)
(define f (lambda (k) (_add 5 (k 6))))
(_add 3 (_add 4 (call-with-current-continuation f)))
