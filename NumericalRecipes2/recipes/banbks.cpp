#include "nr.h"

void NR::banbks(Mat_I_DP &a, const int m1, const int m2, Mat_I_DP &al,
	Vec_I_INT &indx, Vec_IO_DP &b)
{
	int i,j,k,l,mm;
	DP dum;

	int n=a.nrows();
	mm=m1+m2+1;
	l=m1;
	for (k=0;k<n;k++) {
		j=indx[k]-1;
		if (j!=k) SWAP(b[k],b[j]);
		if (l<n) l++;
		for (j=k+1;j<l;j++) b[j] -= al[k][j-k-1]*b[k];
	}
	l=1;
	for (i=n-1;i>=0;i--) {
		dum=b[i];
		for (k=1;k<l;k++) dum -= a[i][k]*b[k+i];
		b[i]=dum/a[i][0];
		if (l<mm) l++;
	}
}
