#include "nr.h"

void NR::spread(const DP y, Vec_IO_DP &yy, const DP x, const int m)
{
	static int nfac[11]={0,1,1,2,6,24,120,720,5040,40320,362880};
	int ihi,ilo,ix,j,nden;
	DP fac;

	int n=yy.size();
	if (m > 10) nrerror("factorial table too small in spread");
	ix=int(x);
	if (x == DP(ix)) yy[ix-1] += y;
	else {
		ilo=MIN(MAX(int(x-0.5*m),0),int(n-m));
		ihi=ilo+m;
		nden=nfac[m];
		fac=x-ilo-1;
		for (j=ilo+1;j<ihi;j++) fac *= (x-j-1);
		yy[ihi-1] += y*fac/(nden*(x-ihi));
		for (j=ihi-1;j>ilo;j--) {
			nden=(nden/(j-ilo))*(j-ihi);
			yy[j-1] += y*fac/(nden*(x-j));
		}
	}
}
