#include <cmath>
#include "nr.h"
using namespace std;

DP NR::rtbis(DP func(const DP), const DP x1, const DP x2, const DP xacc)
{
	const int JMAX=40;
	int j;
	DP dx,f,fmid,xmid,rtb;

	f=func(x1);
	fmid=func(x2);
	if (f*fmid >= 0.0) nrerror("Root must be bracketed for bisection in rtbis");
	rtb = f < 0.0 ? (dx=x2-x1,x1) : (dx=x1-x2,x2);
	for (j=0;j<JMAX;j++) {
		fmid=func(xmid=rtb+(dx *= 0.5));
		if (fmid <= 0.0) rtb=xmid;
		if (fabs(dx) < xacc || fmid == 0.0) return rtb;
	}
	nrerror("Too many bisections in rtbis");
	return 0.0;
}
