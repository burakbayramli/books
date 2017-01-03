#include "nr.h"

void NR::rsolv(Mat_I_DP &a, Vec_I_DP &d, Vec_IO_DP &b)
{
	int i,j;
	DP sum;

	int n=a.nrows();
	b[n-1] /= d[n-1];
	for (i=n-2;i>=0;i--) {
		for (sum=0.0,j=i+1;j<n;j++) sum += a[i][j]*b[j];
		b[i]=(b[i]-sum)/d[i];
	}
}
