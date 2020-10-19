(define shadow (lambda (a0 a1 a2 a3) (define a-1 a0) (define a0 (_add a0 a1)) (define a01 (_add a0 a1)) (_add a01 a2)))
(shadow a0 a1 a2 a3)
