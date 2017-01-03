#include "nr.h"

void NR::pcshft(const DP a, const DP b, Vec_IO_DP &d)
{
	int k,j;
	DP fac,cnst;

	int n=d.size();
	cnst=2.0/(b-a);
	fac=cnst;
	for (j=1;j<n;j++) {
		d[j] *= fac;
		fac *= cnst;
	}
	cnst=0.5*(a+b);
	for (j=0;j<=n-2;j++)
		for (k=n-2;k>=j;k--)
			d[k] -= cnst*d[k+1];
}
