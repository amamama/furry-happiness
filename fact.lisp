 (define fact (lambda (n) (if (eq n 0) 1 (_mul n (fact (_sub n 1))))))
 (define fact3 (fact 3))
 (fact fact3)
