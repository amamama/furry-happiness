#define A(n) A##n
#define arg(n) (car_cdnr(root, n))
#define e(r) (eval(r, frame))
#define decl_arg(n) cell_p a##n = e(arg(n))
#define A0
#define A1 A0; decl_arg(1)
#define A2 A1; decl_arg(2)
#define A3 A2; decl_arg(3)
#define A4 A3; decl_arg(4)
#define A5 A4; decl_arg(5)
#define A6 A5; decl_arg(6)
#define a(n) a##n
#define arith_biop(a, op, b) ((cell_p)((atom_to_int(a)) op (atom_to_int(b))))

begin(keyword, KEYWORD)
keyword("'", q, 0,
	copy(arg(1), -1)
)
keyword("quote", quote, 0,
	copy(arg(1), -1)
)
keyword("if", if, 0,
	e(arg(1))?e(arg(2)):e(arg(3))
)

keyword("lambda", lambda, 0,
	alloc_cell(root, frame, FUNC)
)

//現在のフレームの環境に変数を追加する
//破壊的変更によって再帰関数の環境定義が簡単になる
keyword("define", define, 0,
	(car(frame) = cons(cons(arg(1), e(arg(2))), car(frame)), NULL)
)

// Lisp on Lispをするため（defineするため，↑のdefineは破壊的代入を行う）に追加
keyword("set!", set, 0,
	cdr(get_from_frame(arg(1), frame)) = e(arg(2))
)
// closure 変換をするために導入
keyword("クロージャ作成", gen_closure, 2,
	alloc_cell(car(a(1)), a(2), CLO)
)

keyword("クロージャ適用", apply_closure, 0,
	//eval_body(car(a(1)), cons(NULL, make_new_frame(car(a(1)), cons(cdr(a(1)), eval_args(cdr(cdr(root)), frame)), frame)))
	apply_closure(e(arg(1)), cdr(cdr(root)), frame)
)
/*
keyword("apply-closure", apply_clo, 1,
	eval_body(car(a(1)), cons(NULL, make_new_frame(car(a(1)), cons(cdr(a(1)), eval_args(cdr(cdr(root)), frame)), frame)))
)
*/
end(keyword, KEYWORD)

begin(predefined, PREDEFINED)
predefined("atom?", atom_q, 1, is(ATOM, a(1))?cons(NULL, NULL):NULL) //to be obsolete for typing
predefined("atom", atom, 1, is(ATOM, a(1))?cons(NULL, NULL):NULL) //to be obsolete for typing
predefined("number?", number_q, 1, is(NUMBER, a(1))?cons(NULL, NULL):NULL) //to be obsolete for typing
predefined("eq?", eq_q, 2, is_same_atom(a(1), a(2)) || a(1) == a(2)?cons(NULL, NULL):NULL)
predefined("eq", eq, 2, is_same_atom(a(1), a(2)) || a(1) == a(2)?cons(NULL, NULL):NULL)
predefined("gt?", gt_q, 2, atom_to_int(a(1)) >= atom_to_int(a(2))?cons(NULL, NULL):NULL)
predefined("gt", gt, 2, atom_to_int(a(1)) >= atom_to_int(a(2))?cons(NULL, NULL):NULL)
predefined("set-car!", set_car, 2, car(a(1)) = a(2))
predefined("set-cdr!", set_cdr, 2, cdr(a(1)) = a(2))
predefined("cons", cons, 2, cons(a(1), a(2))) // 'a -> list 'a -> list 'a
predefined("car", car, 1, car(a(1))) // list 'a -> 'a
predefined("cdr", cdr, 1, cdr(a(1))) // list 'a -> list 'a
predefined("pair", pair, 2, cons(a(1), a(2))) //'a -> 'b -> pair 'a 'b
predefined("fst", fst, 1, car(a(1))) // pair 'a 'b -> 'a
predefined("snd", snd, 1, cdr(a(1))) // pair 'a 'b -> 'b
predefined("_add", add, 2, alloc_cell(arith_biop(a(1), +, a(2)), NULL, NUMBER))
predefined("_sub", sub, 2, alloc_cell(arith_biop(a(1), -, a(2)), NULL, NUMBER))
predefined("_mul", mul, 2, alloc_cell(arith_biop(a(1), *, a(2)), NULL, NUMBER))
predefined("_div", div, 2, alloc_cell(arith_biop(a(1), /, a(2)), NULL, NUMBER))
predefined("_mod", mod, 2, alloc_cell(arith_biop(a(1), %, a(2)), NULL, NUMBER))

predefined("move末尾to通常の引数の場所", move_arg, 2, move_arg(a(1), atom_to_int(a(2))))
predefined("to左辺値", to_lvalue, 2, cdnr(a(1), atom_to_int(a(2))))
predefined("記憶域確保", alloc_local, 2, cdr(a(1)) = a(2))
predefined("上の環境へ", up_env, 2, canr(a(1), atom_to_int(a(2))))
predefined("変数の取得", get_value, 2, car_cdnr(a(1), atom_to_int(a(2))))
predefined("変数への代入", assign_value, 2, car(a(1)) = a(2))

predefined("canr", canr, 2, canr(a(1), atom_to_int(a(2))))
predefined("cdnr", cdnr, 2, cdnr(a(1), atom_to_int(a(2))))
predefined("car-cdnr", car_cdnr, 2, car_cdnr(a(1), atom_to_int(a(2))))
end(predefined, PREDEFINED)

#undef A
#undef arg
#undef decl_arg
#define A0
#undef A1
#undef A2
#undef A3
#undef A4
#undef A5
#undef A6
#undef a
#undef arith_biop
