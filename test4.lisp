(define a 0) ((lambda (_ f) (f)) (if 0 (set! a 1) (set! a 2)) (lambda () a))
