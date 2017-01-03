#include <cmath>
#include "nr.h"
using namespace std;

DP NR::zriddr(DP func(const DP), const DP x1, const DP x2, const DP xacc)
{
	const int MAXIT=60;
	const DP UNUSED=-1.11e30;
	int j;
	DP ans,fh,fl,fm,fnew,s,xh,xl,xm,xnew;

	fl=func(x1);
	fh=func(x2);
	if ((fl > 0.0 && fh < 0.0) || (fl < 0.0 && fh > 0.0)) {
		xl=x1;
		xh=x2;
		ans=UNUSED;
		for (j=0;j<MAXIT;j++) {
			xm=0.5*(xl+xh);
			fm=func(xm);
			s=sqrt(fm*fm-fl*fh);
			if (s == 0.0) return ans;
			xnew=xm+(xm-xl)*((fl >= fh ? 1.0 : -1.0)*fm/s);
			if (fabs(xnew-ans) <= xacc) return ans;
			ans=xnew;
			fnew=func(ans);
			if (fnew == 0.0) return ans;
			if (SIGN(fm,fnew) != fm) {
				xl=xm;
				fl=fm;
				xh=ans;
				fh=fnew;
			} else if (SIGN(fl,fnew) != fl) {
				xh=ans;
				fh=fnew;
			} else if (SIGN(fh,fnew) != fh) {
				xl=ans;
				fl=fnew;
			} else nrerror("never get here.");
			if (fabs(xh-xl) <= xacc) return ans;
		}
		nrerror("zriddr exceed maximum iterations");
	}
	else {
		if (fl == 0.0) return x1;
		if (fh == 0.0) return x2;
		nrerror("root must be bracketed in zriddr.");
	}
	return 0.0;
}
