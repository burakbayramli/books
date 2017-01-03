#include "nr.h"

void NR::jacobn_s(const DP x, Vec_I_DP &y, Vec_O_DP &dfdx, Mat_O_DP &dfdy)
{
	int i;

	int n=y.size();
	for (i=0;i<n;i++) dfdx[i]=0.0;
	dfdy[0][0] = -0.013-1000.0*y[2];
	dfdy[0][1] = 0.0;
	dfdy[0][2] = -1000.0*y[0];
	dfdy[1][0] = 0.0;
	dfdy[1][1] = -2500.0*y[2];
	dfdy[1][2] = -2500.0*y[1];
	dfdy[2][0] = -0.013-1000.0*y[2];
	dfdy[2][1] = -2500.0*y[2];
	dfdy[2][2] = -1000.0*y[0]-2500.0*y[1];
}

void NR::derivs_s(const DP x, Vec_I_DP &y, Vec_O_DP &dydx)
{
	dydx[0] = -0.013*y[0]-1000.0*y[0]*y[2];
	dydx[1] = -2500.0*y[1]*y[2];
	dydx[2] = -0.013*y[0]-1000.0*y[0]*y[2]-2500.0*y[1]*y[2];
}
