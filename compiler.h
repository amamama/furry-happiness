#ifndef COMPILER_H
#define COMPILER_H

cell_p to_cps(cell_p, cell_p);

#ifdef CPS2
#define to_cps(root, cont) (to_cps2(root))
#endif

#endif