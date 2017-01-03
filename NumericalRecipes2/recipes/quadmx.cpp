#include <cmath>
#include "nr.h"
using namespace std;

DP x;

void NR::quadmx(Mat_O_DP &a)
{
	const DP PI=3.14159263589793238;
	int j,k;
	DP h,xx,cx;

	int n=a.nrows();
	Vec_DP wt(n);
	h=PI/(n-1);
	for (j=0;j<n;j++) {
		x=xx=j*h;
		wwghts(wt,h,kermom);
		cx=cos(xx);
		for (k=0;k<n;k++)
			a[j][k]=wt[k]*cx*cos(k*h);
		++a[j][j];
	}
}
