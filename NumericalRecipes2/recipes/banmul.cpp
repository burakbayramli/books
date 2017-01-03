#include "nr.h"

void NR::banmul(Mat_I_DP &a, const int m1, const int m2, Vec_I_DP &x,
	Vec_O_DP &b)
{
	int i,j,k,tmploop;

	int n=a.nrows();
	for (i=0;i<n;i++) {
		k=i-m1;
		tmploop=MIN(m1+m2+1,int(n-k));
		b[i]=0.0;
		for (j=MAX(0,-k);j<tmploop;j++) b[i] += a[i][j]*x[j+k];
	}
}
