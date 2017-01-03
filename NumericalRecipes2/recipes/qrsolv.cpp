#include "nr.h"

void NR::qrsolv(Mat_I_DP &a, Vec_I_DP &c, Vec_I_DP &d, Vec_IO_DP &b)
{
	int i,j;
	DP sum,tau;

	int n=a.nrows();
	for (j=0;j<n-1;j++) {
		for (sum=0.0,i=j;i<n;i++) sum += a[i][j]*b[i];
		tau=sum/c[j];
		for (i=j;i<n;i++) b[i] -= tau*a[i][j];
	}
	rsolv(a,d,b);
}
