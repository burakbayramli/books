#include "nr.h"

void NR::rkck(Vec_I_DP &y, Vec_I_DP &dydx, const DP x,
	const DP h, Vec_O_DP &yout, Vec_O_DP &yerr,
	void derivs(const DP, Vec_I_DP &, Vec_O_DP &))
{
	static const DP a2=0.2, a3=0.3, a4=0.6, a5=1.0, a6=0.875,
		b21=0.2, b31=3.0/40.0, b32=9.0/40.0, b41=0.3, b42 = -0.9,
		b43=1.2, b51 = -11.0/54.0, b52=2.5, b53 = -70.0/27.0,
		b54=35.0/27.0, b61=1631.0/55296.0, b62=175.0/512.0,
		b63=575.0/13824.0, b64=44275.0/110592.0, b65=253.0/4096.0,
		c1=37.0/378.0, c3=250.0/621.0, c4=125.0/594.0, c6=512.0/1771.0,
		dc1=c1-2825.0/27648.0, dc3=c3-18575.0/48384.0,
		dc4=c4-13525.0/55296.0, dc5 = -277.00/14336.0, dc6=c6-0.25;
	int i;

	int n=y.size();
	Vec_DP ak2(n),ak3(n),ak4(n),ak5(n),ak6(n),ytemp(n);
	for (i=0;i<n;i++)
		ytemp[i]=y[i]+b21*h*dydx[i];
	derivs(x+a2*h,ytemp,ak2);
	for (i=0;i<n;i++)
		ytemp[i]=y[i]+h*(b31*dydx[i]+b32*ak2[i]);
	derivs(x+a3*h,ytemp,ak3);
	for (i=0;i<n;i++)
		ytemp[i]=y[i]+h*(b41*dydx[i]+b42*ak2[i]+b43*ak3[i]);
	derivs(x+a4*h,ytemp,ak4);
	for (i=0;i<n;i++)
		ytemp[i]=y[i]+h*(b51*dydx[i]+b52*ak2[i]+b53*ak3[i]+b54*ak4[i]);
	derivs(x+a5*h,ytemp,ak5);
	for (i=0;i<n;i++)
		ytemp[i]=y[i]+h*(b61*dydx[i]+b62*ak2[i]+b63*ak3[i]+b64*ak4[i]+b65*ak5[i]);
	derivs(x+a6*h,ytemp,ak6);
	for (i=0;i<n;i++)
		yout[i]=y[i]+h*(c1*dydx[i]+c3*ak3[i]+c4*ak4[i]+c6*ak6[i]);
	for (i=0;i<n;i++)
		yerr[i]=h*(dc1*dydx[i]+dc3*ak3[i]+dc4*ak4[i]+dc5*ak5[i]+dc6*ak6[i]);
}
