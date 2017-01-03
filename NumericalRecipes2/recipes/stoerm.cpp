#include "nr.h"

void NR::stoerm(Vec_I_DP &y, Vec_I_DP &d2y, const DP xs,
	const DP htot, const int nstep, Vec_O_DP &yout,
	void derivs(const DP, Vec_I_DP &, Vec_O_DP &))
{
	int i,nn,n,neqns;
	DP h,h2,halfh,x;

	int nv=y.size();
	Vec_DP ytemp(nv);
	h=htot/nstep;
	halfh=0.5*h;
	neqns=nv/2;
	for (i=0;i<neqns;i++) {
		n=neqns+i;
		ytemp[i]=y[i]+(ytemp[n]=h*(y[n]+halfh*d2y[i]));
	}
	x=xs+h;
	derivs(x,ytemp,yout);
	h2=h*h;
	for (nn=1;nn<nstep;nn++) {
		for (i=0;i<neqns;i++)
			ytemp[i] += (ytemp[neqns+i] += h2*yout[i]);
		x += h;
		derivs(x,ytemp,yout);
	}
	for (i=0;i<neqns;i++) {
		n=neqns+i;
		yout[n]=ytemp[n]/h+halfh*yout[i];
		yout[i]=ytemp[i];
	}
}
