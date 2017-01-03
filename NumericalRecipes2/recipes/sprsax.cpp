#include "nr.h"

void NR::sprsax(Vec_I_DP &sa, Vec_I_INT &ija, Vec_I_DP &x, Vec_O_DP &b)
{
	int i,k;

	int n=x.size();
	if (ija[0] != n+1)
		nrerror("sprsax: mismatched vector and matrix");
	for (i=0;i<n;i++) {
		b[i]=sa[i]*x[i];
		for (k=ija[i];k<ija[i+1];k++) {
			b[i] += sa[k]*x[ija[k]];
		}
	}
}
