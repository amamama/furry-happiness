(define even (lambda (n) (if (eq n 0) 0 (odd (_sub n 1)))))
(define odd (lambda (n) (if (eq n 0) 1 (even (_sub n 1)))))
(odd 101)
