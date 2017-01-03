#include <complex>
#include "nr.h"
using namespace std;

void NR::zrhqr(Vec_I_DP &a, Vec_O_CPLX_DP &rt)
{
	int j,k;
	complex<DP> x;

	int m=a.size()-1;
	Mat_DP hess(m,m);
	for (k=0;k<m;k++) {
		hess[0][k] = -a[m-k-1]/a[m];
		for (j=1;j<m;j++) hess[j][k]=0.0;
		if (k != m-1) hess[k+1][k]=1.0;
	}
	balanc(hess);
	hqr(hess,rt);
	for (j=1;j<m;j++) {
		x=rt[j];
		for (k=j-1;k>=0;k--) {
			if (real(rt[k]) <= real(x)) break;
			rt[k+1]=rt[k];
		}
		rt[k+1]=x;
	}
}
