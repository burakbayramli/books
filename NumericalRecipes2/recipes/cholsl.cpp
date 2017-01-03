#include "nr.h"

void NR::cholsl(Mat_I_DP &a, Vec_I_DP &p, Vec_I_DP &b, Vec_O_DP &x)
{
	int i,k;
	DP sum;

	int n=a.nrows();
	for (i=0;i<n;i++) {
		for (sum=b[i],k=i-1;k>=0;k--) sum -= a[i][k]*x[k];
		x[i]=sum/p[i];
	}
	for (i=n-1;i>=0;i--) {
		for (sum=x[i],k=i+1;k<n;k++) sum -= a[k][i]*x[k];
		x[i]=sum/p[i];
	}
}
