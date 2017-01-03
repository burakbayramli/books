#include "nr.h"

void NR::pccheb(Vec_I_DP &d, Vec_O_DP &c)
{
	int j,jm,jp,k;
	DP fac,pow;

	int n=d.size();
	pow=1.0;
	c[0]=2.0*d[0];
	for (k=1;k<n;k++) {
		c[k]=0.0;
		fac=d[k]/pow;
		jm=k;
		jp=1;
		for (j=k;j>=0;j-=2,jm--,jp++) {
			c[j] += fac;
			fac *= DP(jm)/DP(jp);
		}
		pow += pow;
	}
}
