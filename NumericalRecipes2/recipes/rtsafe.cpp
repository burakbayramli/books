#include <cmath>
#include "nr.h"
using namespace std;

DP NR::rtsafe(void funcd(const DP, DP &, DP &), const DP x1, const DP x2,
	const DP xacc)
{
	const int MAXIT=100;
	int j;
	DP df,dx,dxold,f,fh,fl,temp,xh,xl,rts;

	funcd(x1,fl,df);
	funcd(x2,fh,df);
	if ((fl > 0.0 && fh > 0.0) || (fl < 0.0 && fh < 0.0))
		nrerror("Root must be bracketed in rtsafe");
	if (fl == 0.0) return x1;
	if (fh == 0.0) return x2;
	if (fl < 0.0) {
		xl=x1;
		xh=x2;
	} else {
		xh=x1;
		xl=x2;
	}
	rts=0.5*(x1+x2);
	dxold=fabs(x2-x1);
	dx=dxold;
	funcd(rts,f,df);
	for (j=0;j<MAXIT;j++) {
		if ((((rts-xh)*df-f)*((rts-xl)*df-f) > 0.0)
			|| (fabs(2.0*f) > fabs(dxold*df))) {
			dxold=dx;
			dx=0.5*(xh-xl);
			rts=xl+dx;
			if (xl == rts) return rts;
		} else {
			dxold=dx;
			dx=f/df;
			temp=rts;
			rts -= dx;
			if (temp == rts) return rts;
		}
		if (fabs(dx) < xacc) return rts;
		funcd(rts,f,df);
		if (f < 0.0)
			xl=rts;
		else
			xh=rts;
	}
	nrerror("Maximum number of iterations exceeded in rtsafe");
	return 0.0;
}
