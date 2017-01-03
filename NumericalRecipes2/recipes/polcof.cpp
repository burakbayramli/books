#include <cmath>
#include "nr.h"
using namespace std;

void NR::polcof(Vec_I_DP &xa, Vec_I_DP &ya, Vec_O_DP &cof)
{
	int k,j,i;
	DP xmin,dy;

	int n=xa.size();
	Vec_DP x(n),y(n);
	for (j=0;j<n;j++) {
		x[j]=xa[j];
		y[j]=ya[j];
	}
	for (j=0;j<n;j++) {
		Vec_DP x_t(n-j),y_t(n-j);
		for (k=0;k<n-j;k++) {
			x_t[k]=x[k];
			y_t[k]=y[k];
		}
		polint(x_t,y_t,0.0,cof[j],dy);
		xmin=1.0e38;
		k = -1;
		for (i=0;i<n-j;i++) {
			if (fabs(x[i]) < xmin) {
				xmin=fabs(x[i]);
				k=i;
			}
			if (x[i] != 0.0)
				y[i]=(y[i]-cof[j])/x[i];
		}
		for (i=k+1;i<n-j;i++) {
			y[i-1]=y[i];
			x[i-1]=x[i];
		}
	}
}
