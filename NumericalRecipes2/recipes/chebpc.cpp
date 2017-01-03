#include "nr.h"

void NR::chebpc(Vec_I_DP &c, Vec_O_DP &d)
{
	int k,j;
	DP sv;

	int n=c.size();
	Vec_DP dd(n);
	for (j=0;j<n;j++) d[j]=dd[j]=0.0;
	d[0]=c[n-1];
	for (j=n-2;j>0;j--) {
		for (k=n-j;k>0;k--) {
			sv=d[k];
			d[k]=2.0*d[k-1]-dd[k];
			dd[k]=sv;
		}
		sv=d[0];
		d[0] = -dd[0]+c[j];
		dd[0]=sv;
	}
	for (j=n-1;j>0;j--)
		d[j]=d[j-1]-dd[j];
	d[0] = -dd[0]+0.5*c[0];
}
