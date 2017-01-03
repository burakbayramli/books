#include "nr.h"

void NR::orthog(Vec_I_DP &anu, Vec_I_DP &alpha, Vec_I_DP &beta, Vec_O_DP &a,
	Vec_O_DP &b)
{
	int k,l,looptmp;

	int n=a.size();
	Mat_DP sig(2*n+1,2*n+1);
	looptmp=2*n;
	for (l=2;l<looptmp;l++) sig[0][l]=0.0;
	looptmp++;
	for (l=1;l<looptmp;l++) sig[1][l]=anu[l-1];
	a[0]=alpha[0]+anu[1]/anu[0];
	b[0]=0.0;
	for (k=2;k<n+1;k++) {
		looptmp=2*n-k+2;
		for (l=k;l<looptmp;l++) {
			sig[k][l]=sig[k-1][l+1]+(alpha[l-1]-a[k-2])*sig[k-1][l]
				-b[k-2]*sig[k-2][l]+beta[l-1]*sig[k-1][l-1];
		}
		a[k-1]=alpha[k-1]+sig[k][k+1]/sig[k][k]-sig[k-1][k]/sig[k-1][k-1];
		b[k-1]=sig[k][k]/sig[k-1][k-1];
	}
}
