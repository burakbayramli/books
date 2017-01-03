#include "nr.h"

void NR::mprove(Mat_I_DP &a, Mat_I_DP &alud, Vec_I_INT &indx, Vec_I_DP &b,
	Vec_IO_DP &x)
{
	int i,j;

	int n=x.size();
	Vec_DP r(n);
	for (i=0;i<n;i++) {
		long double sdp = -b[i];
		for (j=0;j<n;j++)
			sdp += (long double) a[i][j]*(long double) x[j];
		r[i]=sdp;
	}
	lubksb(alud,indx,r);
	for (i=0;i<n;i++) x[i] -= r[i];
}
