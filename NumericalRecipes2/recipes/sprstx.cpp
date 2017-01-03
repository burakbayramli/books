#include "nr.h"

void NR::sprstx(Vec_I_DP &sa, Vec_I_INT &ija, Vec_I_DP &x, Vec_O_DP &b)
{
	int i,j,k;

	int n=x.size();
	if (ija[0] != (n+1))
		nrerror("mismatched vector and matrix in sprstx");
	for (i=0;i<n;i++) b[i]=sa[i]*x[i];
	for (i=0;i<n;i++) {
		for (k=ija[i];k<ija[i+1];k++) {
			j=ija[k];
			b[j] += sa[k]*x[i];
		}
	}
}
