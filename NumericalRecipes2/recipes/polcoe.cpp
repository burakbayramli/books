#include "nr.h"

void NR::polcoe(Vec_I_DP &x, Vec_I_DP &y, Vec_O_DP &cof)
{
	int k,j,i;
	DP phi,ff,b;

	int n=x.size();
	Vec_DP s(n);
	for (i=0;i<n;i++) s[i]=cof[i]=0.0;
	s[n-1]= -x[0];
	for (i=1;i<n;i++) {
		for (j=n-1-i;j<n-1;j++)
			s[j] -= x[i]*s[j+1];
		s[n-1] -= x[i];
	}
	for (j=0;j<n;j++) {
		phi=n;
		for (k=n-1;k>0;k--)
			phi=k*s[k]+x[j]*phi;
		ff=y[j]/phi;
		b=1.0;
		for (k=n-1;k>=0;k--) {
			cof[k] += b*ff;
			b=s[k]+x[j]*b;
		}
	}
}
