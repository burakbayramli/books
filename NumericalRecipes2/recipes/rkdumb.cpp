#include "nr.h"

extern Vec_DP *xx_p;
extern Mat_DP *y_p;

void NR::rkdumb(Vec_I_DP &vstart, const DP x1, const DP x2,
	void derivs(const DP, Vec_I_DP &, Vec_O_DP &))
{
	int i,k;
	DP x,h;

	Vec_DP &xx=*xx_p;
	Mat_DP &y=*y_p;
	int nvar=y.nrows();
	int nstep=y.ncols()-1;
	Vec_DP v(nvar),vout(nvar),dv(nvar);
	for (i=0;i<nvar;i++) {
		v[i]=vstart[i];
		y[i][0]=v[i];
	}
	xx[0]=x1;
	x=x1;
	h=(x2-x1)/nstep;
	for (k=0;k<nstep;k++) {
		derivs(x,v,dv);
		rk4(v,dv,x,h,vout,derivs);
		if (x+h == x)
			nrerror("Step size too small in routine rkdumb");
		x += h;
		xx[k+1]=x;
		for (i=0;i<nvar;i++) {
			v[i]=vout[i];
			y[i][k+1]=v[i];
		}
	}
}
