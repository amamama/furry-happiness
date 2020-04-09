#ifndef UTIL_H
#define UTIL_H

#include "pl.h"

cell_p alloc_cell(cell_p, cell_p, cell_type);
cell_p copy(cell_p, int);
cell_p append(cell_p, cell_p);
cell_p car_cdnr(cell_p, unsigned int);

#endif
