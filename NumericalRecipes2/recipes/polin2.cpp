#include "nr.h"

void NR::polin2(Vec_I_DP &x1a, Vec_I_DP &x2a, Mat_I_DP &ya, const DP x1,
	const DP x2, DP &y, DP &dy)
{
	int j,k;

	int m=x1a.size();
	int n=x2a.size();
	Vec_DP ymtmp(m),ya_t(n);
	for (j=0;j<m;j++) {
		for (k=0;k<n;k++) ya_t[k]=ya[j][k];
		polint(x2a,ya_t,x2,ymtmp[j],dy);
	}
	polint(x1a,ymtmp,x1,y,dy);
}
