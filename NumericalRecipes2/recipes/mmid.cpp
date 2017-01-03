#include "nr.h"

void NR::mmid(Vec_I_DP &y, Vec_I_DP &dydx, const DP xs, const DP htot,
	const int nstep, Vec_O_DP &yout,
	void derivs(const DP, Vec_I_DP &, Vec_O_DP &))
{
	int i,n;
	DP x,swap,h2,h;

	int nvar=y.size();
	Vec_DP ym(nvar),yn(nvar);
	h=htot/nstep;
	for (i=0;i<nvar;i++) {
		ym[i]=y[i];
		yn[i]=y[i]+h*dydx[i];
	}
	x=xs+h;
	derivs(x,yn,yout);
	h2=2.0*h;
	for (n=1;n<nstep;n++) {
		for (i=0;i<nvar;i++) {
			swap=ym[i]+h2*yout[i];
			ym[i]=yn[i];
			yn[i]=swap;
		}
		x += h;
		derivs(x,yn,yout);
	}
	for (i=0;i<nvar;i++)
		yout[i]=0.5*(ym[i]+yn[i]+h*yout[i]);
}
