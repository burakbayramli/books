#include <cmath>
#include "nr.h"
using namespace std;

void NR::gaucof(Vec_IO_DP &a, Vec_IO_DP &b, const DP amu0, Vec_O_DP &x,
	Vec_O_DP &w)
{
	int i,j;

	int n=a.size();
	Mat_DP z(n,n);
	for (i=0;i<n;i++) {
		if (i != 0) b[i]=sqrt(b[i]);
		for (j=0;j<n;j++) z[i][j]=DP(i == j);
	}
	tqli(a,b,z);
	eigsrt(a,z);
	for (i=0;i<n;i++) {
		x[i]=a[i];
		w[i]=amu0*z[0][i]*z[0][i];
	}
}
