#include "nr.h"

void NR::poldiv(Vec_I_DP &u, Vec_I_DP &v, Vec_O_DP &q, Vec_O_DP &r)
{
	int k,j;

	int n=u.size()-1;
	int nv=v.size()-1;
	for (j=0;j<=n;j++) {
		r[j]=u[j];
		q[j]=0.0;
	}
	for (k=n-nv;k>=0;k--) {
		q[k]=r[nv+k]/v[nv];
		for (j=nv+k-1;j>=k;j--) r[j] -= q[k]*v[j-k];
	}
	for (j=nv;j<=n;j++) r[j]=0.0;
}
