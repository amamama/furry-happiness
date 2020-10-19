(define make-counter (lambda (n) (define count (_sub n 1)) (cons (lambda () (set! count (_add count 1))) (lambda () (set! count 0)))))
(define counter3 (make-counter 3))
(define counter7 (make-counter 7))
(cons ((car counter3)) (cons ((car counter7)) (cons ((cdr counter3)) (cons ((car counter3)) (cons ((car counter7)) '())))))
