#include "nr.h"

void NR::simpr(Vec_I_DP &y, Vec_I_DP &dydx, Vec_I_DP &dfdx, Mat_I_DP &dfdy,
	const DP xs, const DP htot, const int nstep, Vec_O_DP &yout,
	void derivs(const DP, Vec_I_DP &, Vec_O_DP &))
{
	int i,j,nn;
	DP d,h,x;

	int n=y.size();
	Mat_DP a(n,n);
	Vec_INT indx(n);
	Vec_DP del(n),ytemp(n);
	h=htot/nstep;
	for (i=0;i<n;i++) {
		for (j=0;j<n;j++) a[i][j] = -h*dfdy[i][j];
		++a[i][i];
	}
	ludcmp(a,indx,d);
	for (i=0;i<n;i++)
		yout[i]=h*(dydx[i]+h*dfdx[i]);
	lubksb(a,indx,yout);
	for (i=0;i<n;i++)
		ytemp[i]=y[i]+(del[i]=yout[i]);
	x=xs+h;
	derivs(x,ytemp,yout);
	for (nn=2;nn<=nstep;nn++) {
		for (i=0;i<n;i++)
			yout[i]=h*yout[i]-del[i];
		lubksb(a,indx,yout);
		for (i=0;i<n;i++) ytemp[i] += (del[i] += 2.0*yout[i]);
		x += h;
		derivs(x,ytemp,yout);
	}
	for (i=0;i<n;i++)
		yout[i]=h*yout[i]-del[i];
	lubksb(a,indx,yout);
	for (i=0;i<n;i++)
		yout[i] += ytemp[i];
}
