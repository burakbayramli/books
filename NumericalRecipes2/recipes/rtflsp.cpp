#include <cmath>
#include "nr.h"
using namespace std;

DP NR::rtflsp(DP func(const DP), const DP x1, const DP x2, const DP xacc)
{
	const int MAXIT=30;
	int j;
	DP fl,fh,xl,xh,dx,del,f,rtf;

	fl=func(x1);
	fh=func(x2);
	if (fl*fh > 0.0) nrerror("Root must be bracketed in rtflsp");
	if (fl < 0.0) {
		xl=x1;
		xh=x2;
	} else {
		xl=x2;
		xh=x1;
		SWAP(fl,fh);
	}
	dx=xh-xl;
	for (j=0;j<MAXIT;j++) {
		rtf=xl+dx*fl/(fl-fh);
		f=func(rtf);
		if (f < 0.0) {
			del=xl-rtf;
			xl=rtf;
			fl=f;
		} else {
			del=xh-rtf;
			xh=rtf;
			fh=f;
		}
		dx=xh-xl;
		if (fabs(del) < xacc || f == 0.0) return rtf;
	}
	nrerror("Maximum number of iterations exceeded in rtflsp");
	return 0.0;
}
