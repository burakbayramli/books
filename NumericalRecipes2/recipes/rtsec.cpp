#include <cmath>
#include "nr.h"
using namespace std;

DP NR::rtsec(DP func(const DP), const DP x1, const DP x2, const DP xacc)
{
	const int MAXIT=30;
	int j;
	DP fl,f,dx,xl,rts;

	fl=func(x1);
	f=func(x2);
	if (fabs(fl) < fabs(f)) {
		rts=x1;
		xl=x2;
		SWAP(fl,f);
	} else {
		xl=x1;
		rts=x2;
	}
	for (j=0;j<MAXIT;j++) {
		dx=(xl-rts)*f/(f-fl);
		xl=rts;
		fl=f;
		rts += dx;
		f=func(rts);
		if (fabs(dx) < xacc || f == 0.0) return rts;
	}
	nrerror("Maximum number of iterations exceeded in rtsec");
	return 0.0;
}
