((lambda (src-list)
	(define ca1r (lambda (l) (car (cdr l))))
	(define ca2r (lambda (l) (car (cdr (cdr l)))))
	(define ca3r (lambda (l) (car (cdr (cdr (cdr l))))))
	(define ca4r (lambda (l) (car (cdr (cdr (cdr (cdr l)))))))
	(define get-from-env
		(lambda (var env)
			(if (eq env (quote ()))
				(quote ())
				(if (eq var (car (car env)))
					(car env)
					(get-from-env var (cdr env))))))
	(define get-from-frame
		(lambda (var frame)
			(if (eq frame (quote ()))
				(quote ())
				((lambda (obj)
					(if (eq obj (quote ()))
						(get-from-frame var (cdr frame))
						obj))
				(get-from-env var (car frame))))))
	(define eval-atom
		(lambda (root frame)
			(if (eq root (quote -cons))
				root
			(if (eq root (quote -car))
				root
			(if (eq root (quote -cdr))
				root
			(if (eq root (quote -eq))
				root
			(if (eq root (quote -atom))
				root
				((lambda (obj)
					(if (eq obj (quote ()))
						(quote error-in-eval-atom)
						(cdr obj)))
				(get-from-frame root frame)))))))))
	(define eval-quote
		(lambda (root frame)
			(ca1r root)))
	(define eval-if
		(lambda (root frame)
			(define pred (car (eval (ca1r root) frame)))
			(car (if pred
				(eval (ca2r root) frame)
				(eval (ca3r root) frame)))))
	(define eval-lambda
		(lambda (root frame)
			(cons root frame)))
	(define eval-define
		(lambda (root frame)
			(define cdr-frame (cdr frame))
			(define old-env (car frame))
			(define new-entry (cons (ca1r root) (car (eval (ca2r root) frame))))
			(define new-env (cons new-entry old-env))
			(cons new-env cdr-frame)))
	(define eval-list
		(lambda (root frame)
			(define car-root (car root))
			(define new-frame
				(if (eq car-root (quote -define))
					(eval-define root frame)
					frame))
			(define new-value
				(if (eq car-root (quote -quote))
					(eval-quote root frame)
				(if (eq car-root (quote -if))
					(eval-if root frame)
				(if (eq car-root (quote -lambda))
					(eval-lambda root frame)
				(if (eq car-root (quote -define))
					(quote ())
					((lambda (evaled-car)
						(define eval-cons
							(lambda (root frame)
								(define a1 (car (eval (ca1r root) frame)))
								(define a2 (car (eval (ca2r root) frame)))
								(cons a1 a2)))
						(define eval-car
							(lambda (root frame)
								(define a1 (car (eval (ca1r root) frame)))
								(car a1)))
						(define eval-cdr
							(lambda (root frame)
								(define a1 (car (eval (ca1r root) frame)))
								(cdr a1)))
						(define eval-atom
							(lambda (root frame)
								(define a1 (car (eval (ca1r root) frame)))
								(atom a1)))
						(define eval-eq
							(lambda (root frame)
								(define a1 (car (eval (ca1r root) frame)))
								(define a2 (car (eval (ca2r root) frame)))
								(eq a1 a2)))
						(if (eq evaled-car (quote -cons))
							(eval-cons root frame)
						(if (eq evaled-car (quote -car))
							(eval-car root frame)
						(if (eq evaled-car (quote -cdr))
							(eval-cdr root frame)
						(if (eq evaled-car (quote -atom))
							(eval-atom root frame)
						(if (eq evaled-car (quote -eq))
							(eval-eq root frame)
							(apply evaled-car (cdr root) frame)))))))
					(car (eval (car root) frame))))))))
			(cons new-value new-frame)))
	(define eval-args
		(lambda (args frame)
			(if (eq args (quote ()))
				(quote ())
				(cons (car (eval (car args) frame)) (eval-args (cdr args) frame)))))
	(define make-new-env
		(lambda (decls args frame)
			(define evaled-args (eval-args args frame))
			(if (eq decls (quote ()))
				(quote ())
				(cons (cons (car decls) (car args)) (make-new-env (cdr decls) (cdr args) frame)))))
	(define make-new-frame
		(lambda (func args frame)
			(define lamb (car func))
			(define lamb-frame (cdr func))
			(define env (make-new-env (ca1r lamb) args frame))
			(cons env lamb-frame)))
	(define apply
		(lambda (func args frame)
		(define new-frame (cons (quote ()) (make-new-frame func args frame)))
		(define body (cdr (cdr (car func))))
		(define apply-aux
			(lambda (body frame)
				(define evaled-body (eval (car body) frame))
				(if (eq (cdr body) (quote ()))
					(car evaled-body)
					(apply-aux (cdr body) (cdr evaled-body)))))
		(apply-aux body frame)))
	(define eval
		(lambda (root frame)
			(if (eq root (quote ()))
				(cons (quote ()) frame)
			(if (atom root)
				(cons (eval-atom root frame) frame)
				(eval-list root frame)))))
	(car (eval src-list (quote ()))))
(quote (-cons (-quote a) (-quote b))))
