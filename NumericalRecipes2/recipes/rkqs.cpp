#include <cmath>
#include "nr.h"
using namespace std;

void NR::rkqs(Vec_IO_DP &y, Vec_IO_DP &dydx, DP &x, const DP htry,
	const DP eps, Vec_I_DP &yscal, DP &hdid, DP &hnext,
	void derivs(const DP, Vec_I_DP &, Vec_O_DP &))
{
	const DP SAFETY=0.9, PGROW=-0.2, PSHRNK=-0.25, ERRCON=1.89e-4;
	int i;
	DP errmax,h,htemp,xnew;

	int n=y.size();
	h=htry;
	Vec_DP yerr(n),ytemp(n);
	for (;;) {
		rkck(y,dydx,x,h,ytemp,yerr,derivs);
		errmax=0.0;
		for (i=0;i<n;i++) errmax=MAX(errmax,fabs(yerr[i]/yscal[i]));
		errmax /= eps;
		if (errmax <= 1.0) break;
		htemp=SAFETY*h*pow(errmax,PSHRNK);
		h=(h >= 0.0 ? MAX(htemp,0.1*h) : MIN(htemp,0.1*h));
		xnew=x+h;
		if (xnew == x) nrerror("stepsize underflow in rkqs");
	}
	if (errmax > ERRCON) hnext=SAFETY*h*pow(errmax,PGROW);
	else hnext=5.0*h;
	x += (hdid=h);
	for (i=0;i<n;i++) y[i]=ytemp[i];
}
