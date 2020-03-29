(
       (lambda (fix) (
               (lambda (sum) (
                       sum 5
               ))
               (fix (lambda (f) (lambda (x) (if (eq x 0) 0 (_add x (f (_sub x 1)))))))
       ))
       (lambda (f) (
               (lambda (x) (f (lambda (y) ((x x) y))))
               (lambda (x) (f (lambda (y) ((x x) y))))
       ))
)
