#include <cmath>
#include "nr.h"
using namespace std;

void usrfun(Vec_I_DP &x, Vec_O_DP &fvec, Mat_O_DP &fjac);

void NR::mnewt(const int ntrial, Vec_IO_DP &x, const DP tolx, const DP tolf)
{
	int k,i;
	DP errx,errf,d;

	int n=x.size();
	Vec_INT indx(n);
	Vec_DP p(n),fvec(n);
	Mat_DP fjac(n,n);
	for (k=0;k<ntrial;k++) {
		usrfun(x,fvec,fjac);
		errf=0.0;
		for (i=0;i<n;i++) errf += fabs(fvec[i]);
		if (errf <= tolf) return;
		for (i=0;i<n;i++) p[i] = -fvec[i];
		ludcmp(fjac,indx,d);
		lubksb(fjac,indx,p);
		errx=0.0;
		for (i=0;i<n;i++) {
			errx += fabs(p[i]);
			x[i] += p[i];
		}
		if (errx <= tolx) return;
	}
	return;
}
