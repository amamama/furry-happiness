#ifndef CPS_H
#define CPS_H

cell_p rewrite_define(cell_p);
cell_p to_cps(cell_p, cell_p);

#ifdef CPS2
#define to_cps(root, cont) (to_cps2(root))
#endif

#endif
