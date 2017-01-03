#include "nr.h"

DP NR::qgaus(DP func(const DP), const DP a, const DP b)
{
	static const DP x[]={0.1488743389816312,0.4333953941292472,
		0.6794095682990244,0.8650633666889845,0.9739065285171717};
	static const DP w[]={0.2955242247147529,0.2692667193099963,
		0.2190863625159821,0.1494513491505806,0.0666713443086881};
	int j;
	DP xr,xm,dx,s;

	xm=0.5*(b+a);
	xr=0.5*(b-a);
	s=0;
	for (j=0;j<5;j++) {
		dx=xr*x[j];
		s += w[j]*(func(xm+dx)+func(xm-dx));
	}
	return s *= xr;
}
