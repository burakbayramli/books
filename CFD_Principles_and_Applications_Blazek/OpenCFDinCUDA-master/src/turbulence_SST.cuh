#ifndef _TURBULENCE_SST_H_
#define _TURBULENCE_SST_H_

#include "Flow_var.h"

void turbulence_SST_kw_before_cuda(double * Amu_t_dev, double * Amu_dev, double *d, double *uu, double *vv, double * T,
	double *U_dev, double *x_dev, double *y_dev, double *x1_dev, double *y1_dev, int mBlock, flow_var & fl, int * transferInt_dev, double * transferDouble_dev);





#endif // !_TURBULENCE_SST_H_
