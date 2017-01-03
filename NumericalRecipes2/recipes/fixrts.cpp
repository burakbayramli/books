#include <cmath>
#include <complex>
#include "nr.h"
using namespace std;

void NR::fixrts(Vec_IO_DP &d)
{
	bool polish=true;
	int i,j;

	int m=d.size();
	Vec_CPLX_DP a(m+1),roots(m);
	a[m]=1.0;
	for (j=0;j<m;j++)
		a[j]= -d[m-1-j];
	zroots(a,roots,polish);
	for (j=0;j<m;j++)
		if (abs(roots[j]) > 1.0)
			roots[j]=1.0/conj(roots[j]);
	a[0]= -roots[0];
	a[1]=1.0;
	for (j=1;j<m;j++) {
		a[j+1]=1.0;
		for (i=j;i>=1;i--)
			a[i]=a[i-1]-roots[j]*a[i];
		a[0]= -roots[j]*a[0];
	}
	for (j=0;j<m;j++)
		d[m-1-j] = -real(a[j]);
}
