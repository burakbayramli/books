#include "nr.h"

void NR::splin2(Vec_I_DP &x1a, Vec_I_DP &x2a, Mat_I_DP &ya, Mat_I_DP &y2a,
	const DP x1, const DP x2, DP &y)
{
	int j,k;

	int m=x1a.size();
	int n=x2a.size();
	Vec_DP ya_t(n),y2a_t(n),yytmp(m),ytmp(m);
	for (j=0;j<m;j++) {
		for (k=0;k<n;k++) {
			ya_t[k]=ya[j][k];
			y2a_t[k]=y2a[j][k];
		}
		splint(x2a,ya_t,y2a_t,x2,yytmp[j]);
	}
	spline(x1a,yytmp,1.0e30,1.0e30,ytmp);
	splint(x1a,yytmp,ytmp,x1,y);
}
