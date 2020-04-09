#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "pl.h"
#include "util.h"
#include "cps.h"

// cps変換のため，
// (lambda (args) pre_bodies (define v e) post_bodies)
// -> (lambda (args) pre_bodies (rewrite_define(<<<(lambda (v) (set! v e) post_bodies)>>>) '()))
// のようにする．
// これによって，ナイーブな前方参照や相互再帰ができなくなる．
/*
((lambda ()
	(define add5 (lambda (x) (_add (id 5) x)))
	(define id (lambda (x) x))
	(add5 10)
	))
*/
// 手動でset!すると動くのでこれで上記の問題はこれでなんとかする
// あとでマクロを作ることができればそれで解決する
/* ((lambda (id)
	(define add5 (lambda (x) (_add (id 5) x)))
	(define id (lambda (x) x))
	(add5 10)
	) (lambda (x) (_add x 3)))
*/
// これは書き換え前と後で結果が変わる．
//
// 変数のシャドウイングで落ちる
/* 

(define shadow (lambda (a0 a1 a2 a3) (define a0 (_add a0 a1)) (define a01 (_add a0 a1)) (_add a01 a2)))
(shadow a0 a1 a2 a3)
*/
cell_p rewrite_define_aux(cell_p root, cell_p outer_args) {
	assert(is_lambda(root));
	cell_p args = union_list(car_cdnr(root, 1), outer_args);
	cell_p body = cdr(cdr(root));
	for(; body && (!is(LIST, car(body)) || (is(LIST, car(body)) && !is_same_string("define", car(car(body))))); body = cdr(body)) {
		car(body) = rewrite_define(car(body), args);
	}
	if(!body) return root;
	cell_p var = car_cdnr(car(body), 1);
	cell_p exp = car_cdnr(car(body), 2);
	cell_p set_exp = app3(str_to_atom("set!"), var, exp);
	cell_p new_lambda = make_lambda(cons(var, NULL), cons(set_exp, cdr(body)));
	new_lambda = rewrite_define(new_lambda, args);
	car(body) = app2(new_lambda, is_member(var, args)?var:nil);
	cdr(body) = NULL;
	return root;
}

cell_p rewrite_define(cell_p root, cell_p args) {
	if(!root) return root;
	switch(cty(root)) {
		case ATOM:
		case NUMBER:
		return root;
		case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_keyword[i](root)) {
					switch(i) {
						case K_lambda: {
							return rewrite_define_aux(root, args);
						}
						case K_define: {
							// this case should not be executed
							assert(false);
							return NULL;
						}
					}
				}
			}
			for(cell_p c = root; c; c = cdr(c)) {
				car(c) = rewrite_define(car(c), args);
			}
			return root;
		} default: {
			assert(false);
		}
	}
}

genvar(cps, "継続")

#ifndef CPS2

// (lambda (args) bodies)
/* (cont
   (lambda (k0 args) to_cps(body1,
   (lambda (_) to_cps(body2,
   (lambda (_) to_cps(body...,
   (lambda (_) to_cps(bodyn, k0)))))))))
   */

cell_p bodies_to_cps(cell_p bodies, cell_p cont_var) {
	if(!cdr(bodies)) return to_cps(car(bodies), cont_var);
	cell_p new_lambda = make_lambda(cons(str_to_atom("_"), NULL), cons(bodies_to_cps(cdr(bodies), cont_var), NULL));
	return to_cps(car(bodies), new_lambda);
}

cell_p apply_to_cps_aux(cell_p exp, cell_p cont_vars, cell_p cont) {
	cell_p body = cdr(exp)?apply_to_cps_aux(cdr(exp), cdr(cont_vars), cont):cont;
	cell_p new_cont = make_lambda(cons(car(cont_vars), NULL), cons(body, NULL));
	return to_cps(car(exp), new_cont);
}
cell_p apply_to_cps(cell_p root, cell_p cont) {
	cell_p cont_vars = NULL;
	for(cell_p e = root; e; e = cdr(e)) {
		cont_vars = cons(genvar_cps(), cont_vars);
	}
	cell_p apply_cont = cons(car(cont_vars), cons(cont, cdr(cont_vars)));
	return apply_to_cps_aux(root, cont_vars, apply_cont);
}

cell_p predefined_to_cps(cell_p root, cell_p cont) {
	cell_p cont_vars = NULL;
	for(cell_p e = cdr(root); e; e = cdr(e)) {
		cont_vars = cons(genvar_cps(), cont_vars);
	}
	cell_p predefined_cont = cons(cont, cons(cons(car(root), cont_vars), NULL));
	return apply_to_cps_aux(cdr(root), cont_vars, predefined_cont);
}

cell_p to_cps(cell_p root, cell_p cont) {
	switch(cty(root)) {
		case ATOM:
		case NUMBER: {
			// v
			// (cont v)
			return cons(cont, cons(root, NULL));
		} case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_keyword[i](root)) {
					switch(i) {
						case K_q:
						case K_quote: {
							// (quote v)
							// (cont (quote v))
							return cons(cont, cons(root, NULL));
						} case K_if: {
							// (if cond t e)
							// ((λk0 .to_cps(cond, (λk1. (if k1 to_cps(t, k0) to_cps(e, k0))))) cont)
							cell_p cond = car_cdnr(root, 1);
							cell_p then_cls = car_cdnr(root, 2);
							cell_p else_cls = car_cdnr(root, 3);
							cell_p new_var0 = genvar_cps();
							cell_p new_var1 = genvar_cps();
							cell_p new_then_cls = to_cps(then_cls, new_var0);
							cell_p new_else_cls = to_cps(else_cls, new_var0);
							cell_p new_if = cons(str_to_atom("if"), cons(new_var1, cons(new_then_cls, cons(new_else_cls, NULL))));
							cell_p new_cont = make_lambda(cons(new_var1, NULL), cons(new_if, NULL));
							cell_p new_lambda = make_lambda(cons(new_var0, NULL), cons(to_cps(cond, new_cont), NULL));
							return cons(new_lambda, cons(cont, NULL));
						} case K_lambda: {
							// (lambda (args) bodies)
							/* (cont
							(lambda (k0 args) to_cps(body1,
									(lambda (k1) to_cps(body2,
										(lambda (k2) to_cps(body...,
											(lambda (kn) to_cps(bodyn, k0)))))))))
							*/
							cell_p new_var = genvar_cps();
							cell_p bodies = cdr(cdr(root));
							cell_p new_args = cons(new_var, car_cdnr(root, 1));
							cell_p new_body = bodies_to_cps(bodies, new_var);
							cell_p new_lambda = make_lambda(new_args, cons(new_body, NULL));
							 return cons(cont, cons(new_lambda, NULL));
						}
						case K_define: {
							// this case should not be appeared
							assert(false);
							return NULL;
						}
						case K_set: {
							//(set! v e)
							// to_cps(e, (lambda (k0) (cont (set! v k0))))
							cell_p var = car_cdnr(root, 1);
							cell_p exp = car_cdnr(root, 2);
							cell_p new_var = genvar_cps();
							cell_p body = cons(cont, cons(cons(str_to_atom("set!"), cons(var, cons(new_var, NULL))), NULL));
							cell_p new_cont = make_lambda(cons(new_var, NULL), cons(body, NULL));
							 return to_cps(exp, new_cont);
						}
					}
				}
			}
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_predefined[i](root)) {
					return predefined_to_cps(root, cont);
				}
			}
			return apply_to_cps(root, cont);
		} default: {
			assert(false);
		}
	}
}

#else

cell_p bodies_to_cps2(cell_p bodies, cell_p cont_var) {
	return app2(to_cps2(car(bodies)), cdr(bodies)?make_lambda(cons(str_to_atom("_"), NULL), cons(bodies_to_cps2(cdr(bodies), cont_var), NULL)):cont_var);
}

cell_p apply_to_cps_aux2(cell_p exp, cell_p cont_vars, cell_p cont) {
	cell_p body = cdr(exp)?apply_to_cps_aux2(cdr(exp), cdr(cont_vars), cont):cont;
	cell_p new_cont = make_lambda(cons(car(cont_vars), NULL), cons(body, NULL));
	return app2(to_cps2(car(exp)), new_cont);
}
cell_p apply_to_cps2(cell_p root, cell_p cont) {
	cell_p cont_vars = NULL;
	for(cell_p e = root; e; e = cdr(e)) {
		cont_vars = cons(genvar_cps(), cont_vars);
	}
	cell_p apply_cont = cons(car(cont_vars), cons(cont, cdr(cont_vars)));
	return apply_to_cps_aux2(root, cont_vars, apply_cont);
}

cell_p predefined_to_cps2(cell_p root, cell_p cont) {
	cell_p cont_vars = NULL;
	for(cell_p e = cdr(root); e; e = cdr(e)) {
		cont_vars = cons(genvar_cps(), cont_vars);
	}
	cell_p predefined_cont = cons(cont, cons(cons(car(root), cont_vars), NULL));
	return apply_to_cps_aux2(cdr(root), cont_vars, predefined_cont);
}

cell_p to_cps2(cell_p root) {
	switch(cty(root)) {
		case ATOM:
		case NUMBER: {
			cell_p k = genvar_cps();
			return make_lambda(cons(k, NULL), cons(app2(k, root), NULL));
		} case LIST: {
			for(size_t i = 0; i < NUM_OF_KEYWORD; i++) {
				if(is_keyword[i](root)) {
					switch(i) {
						case K_q:
						case K_quote: {
							cell_p k = genvar_cps();
							return make_lambda(cons(k, NULL), cons(app2(k, root), NULL));
						} case K_if: {
							cell_p cond = car_cdnr(root, 1);
							cell_p then_cls = car_cdnr(root, 2);
							cell_p else_cls = car_cdnr(root, 3);
							cell_p new_var0 = genvar_cps();
							cell_p new_var1 = genvar_cps();
							cell_p new_then_cls = to_cps(then_cls, new_var0);
							cell_p new_else_cls = to_cps(else_cls, new_var0);
							cell_p new_if = cons(str_to_atom("if"), cons(new_var1, cons(new_then_cls, cons(new_else_cls, NULL))));
							cell_p new_cont = make_lambda(cons(new_var1, NULL), cons(new_if, NULL));
							cell_p new_lambda = make_lambda(cons(new_var0, NULL), cons(to_cps(cond, new_cont), NULL));
							return new_lambda;
						} case K_lambda: {
							cell_p new_var = genvar_cps();
							cell_p bodies = cdr(cdr(root));
							cell_p new_args = cons(new_var, car_cdnr(root, 1));
							cell_p new_body = bodies_to_cps2(bodies, new_var);
							cell_p new_lambda = make_lambda(new_args, cons(new_body, NULL));
							cell_p k = genvar_cps();
							 return make_lambda(cons(k, NULL), cons(app2(k, new_lambda), NULL));
						}
						case K_define: {
							// this case should not be appeared
							assert(false);
							return NULL;
						}
						case K_set: {
							cell_p var = car_cdnr(root, 1);
							cell_p exp = car_cdnr(root, 2);
							cell_p new_var = genvar_cps();
							cell_p k = genvar_cps();
							cell_p body = cons(k, cons(cons(str_to_atom("set!"), cons(var, cons(new_var, NULL))), NULL));
							cell_p new_cont = make_lambda(cons(new_var, NULL), cons(body, NULL));

							cell_p new_lambda = make_lambda(cons(k, NULL), cons(app2(to_cps2(exp), new_cont), NULL));
							 return new_lambda;
						}
					}
				}
			}
			for(size_t i = 0; i < NUM_OF_PREDEFINED; i++) {
				if(is_predefined[i](root)) {
					cell_p k = genvar_cps();
					return make_lambda(cons(k, NULL), cons(predefined_to_cps2(root, k), NULL));
				}
			}
			cell_p k = genvar_cps();
			return make_lambda(cons(k, NULL), cons(apply_to_cps2(root, k), NULL));
		} default: {
			assert(false);
		}
	}
}


#endif
