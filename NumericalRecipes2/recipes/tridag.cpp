#include "nr.h"

void NR::tridag(Vec_I_DP &a, Vec_I_DP &b, Vec_I_DP &c, Vec_I_DP &r, Vec_O_DP &u)
{
	int j;
	DP bet;

	int n=a.size();
	Vec_DP gam(n);
	if (b[0] == 0.0) nrerror("Error 1 in tridag");
	u[0]=r[0]/(bet=b[0]);
	for (j=1;j<n;j++) {
		gam[j]=c[j-1]/bet;
		bet=b[j]-a[j]*gam[j];
		if (bet == 0.0) nrerror("Error 2 in tridag");
		u[j]=(r[j]-a[j]*u[j-1])/bet;
	}
	for (j=(n-2);j>=0;j--)
		u[j] -= gam[j+1]*u[j+1];
}
