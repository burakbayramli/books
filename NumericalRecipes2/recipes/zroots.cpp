#include <cmath>
#include <complex>
#include "nr.h"
using namespace std;

void NR::zroots(Vec_I_CPLX_DP &a, Vec_O_CPLX_DP &roots, const bool &polish)
{
	const DP EPS=1.0e-14;
	int i,its,j,jj;
	complex<DP> x,b,c;

	int m=a.size()-1;
	Vec_CPLX_DP ad(m+1);
	for (j=0;j<=m;j++) ad[j]=a[j];
	for (j=m-1;j>=0;j--) {
		x=0.0;
		Vec_CPLX_DP ad_v(j+2);
		for (jj=0;jj<j+2;jj++) ad_v[jj]=ad[jj];
		laguer(ad_v,x,its);
		if (fabs(imag(x)) <= 2.0*EPS*fabs(real(x)))
			x=complex<DP>(real(x),0.0);
		roots[j]=x;
		b=ad[j+1];
		for (jj=j;jj>=0;jj--) {
			c=ad[jj];
			ad[jj]=b;
			b=x*b+c;
		}
	}
	if (polish)
		for (j=0;j<m;j++)
			laguer(a,roots[j],its);
	for (j=1;j<m;j++) {
		x=roots[j];
		for (i=j-1;i>=0;i--) {
			if (real(roots[i]) <= real(x)) break;
			roots[i+1]=roots[i];
		}
		roots[i+1]=x;
	}
}
