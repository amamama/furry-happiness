#define A(n) A##n
#define arg(n) eval(car_cdnr(root, n), env)
#define decl_arg(n) cell_p a##n = arg(n)
#define A0
#define A1 A0; decl_arg(1)
#define A2 A1; decl_arg(2)
#define A3 A2; decl_arg(3)
#define A4 A3; decl_arg(4)
#define A5 A4; decl_arg(5)
#define A6 A5; decl_arg(6)
#define a(n) a##n

begin(keyword, KEYWORD)
keyword("'", q, 0, cdr(root))
keyword("quote", quote, 0, cdr(root))
keyword("if", if, 0, arg(1)?arg(2):arg(3))
keyword("lambda", lambda, 0, alloc_cell(root, env, FUNC))
end(keyword, KEYWORD)

begin(predefined, PREDEFINED)
predefined("atom", atom, 1, is(ATOM, a(1))?alloc_cell(NULL, NULL, LIST):NULL)
predefined("eq", eq, 2, is_same_atom(a(1), a(2))?alloc_cell(NULL, NULL, LIST):NULL)
predefined("cons", cons, 2, alloc_cell(a(1), a(2), LIST))
predefined("car", car, 1, car(a(1)))
predefined("cdr", cdr, 1, cdr(a(1)))
predefined("_add", add, 2, alloc_cell(arith_biop(a(1), +, a(2)), NULL, NUMBER))
predefined("_sub", sub, 2, alloc_cell(arith_biop(a(1), -, a(2)), NULL, NUMBER))
predefined("_mul", mul, 2, alloc_cell(arith_biop(a(1), *, a(2)), NULL, NUMBER))
predefined("_div", div, 2, alloc_cell(arith_biop(a(1), /, a(2)), NULL, NUMBER))
predefined("_mod", mod, 2, alloc_cell(arith_biop(a(1), %, a(2)), NULL, NUMBER))
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
