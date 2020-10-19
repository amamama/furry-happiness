#ifndef UTIL_H
#define UTIL_H

#include "pl.h"

cell_p alloc_cell(cell_p, cell_p, cell_type);
bool is_dotted_list(cell_p);
size_t length(cell_p);
cell_p copy(cell_p, int);
cell_p append(cell_p, cell_p);
cell_p slice(cell_p, cell_p, cell_p);
cell_p car_cdnr(cell_p, unsigned int);
cell_p canr(cell_p, unsigned int);
cell_p cdnr(cell_p, unsigned int);

#endif
