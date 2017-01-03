#include "nr.h"

void NR::splie2(Vec_I_DP &x1a, Vec_I_DP &x2a, Mat_I_DP &ya, Mat_O_DP &y2a)
{
	int m,n,j,k;

	m=x1a.size();
	n=x2a.size();
	Vec_DP ya_t(n),y2a_t(n);
	for (j=0;j<m;j++) {
		for (k=0;k<n;k++) ya_t[k]=ya[j][k];
		spline(x2a,ya_t,1.0e30,1.0e30,y2a_t);
		for (k=0;k<n;k++) y2a[j][k]=y2a_t[k];
	}
}
