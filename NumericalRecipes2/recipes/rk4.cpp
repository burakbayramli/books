#include "nr.h"

void NR::rk4(Vec_I_DP &y, Vec_I_DP &dydx, const DP x, const DP h,
	Vec_O_DP &yout, void derivs(const DP, Vec_I_DP &, Vec_O_DP &))
{
	int i;
	DP xh,hh,h6;

	int n=y.size();
	Vec_DP dym(n),dyt(n),yt(n);
	hh=h*0.5;
	h6=h/6.0;
	xh=x+hh;
	for (i=0;i<n;i++) yt[i]=y[i]+hh*dydx[i];
	derivs(xh,yt,dyt);
	for (i=0;i<n;i++) yt[i]=y[i]+hh*dyt[i];
	derivs(xh,yt,dym);
	for (i=0;i<n;i++) {
		yt[i]=y[i]+h*dym[i];
		dym[i] += dyt[i];
	}
	derivs(x+h,yt,dyt);
	for (i=0;i<n;i++)
		yout[i]=y[i]+h6*(dydx[i]+dyt[i]+2.0*dym[i]);
}
