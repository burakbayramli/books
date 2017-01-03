#include <cmath>
#include "nr.h"
using namespace std;

void NR::qrdcmp(Mat_IO_DP &a, Vec_O_DP &c, Vec_O_DP &d, bool &sing)
{
	int i,j,k;
	DP scale,sigma,sum,tau;

	int n=a.nrows();
	sing=false;
	for (k=0;k<n-1;k++) {
		scale=0.0;
		for (i=k;i<n;i++) scale=MAX(scale,fabs(a[i][k]));
		if (scale == 0.0) {
			sing=true;
			c[k]=d[k]=0.0;
		} else {
			for (i=k;i<n;i++) a[i][k] /= scale;
			for (sum=0.0,i=k;i<n;i++) sum += SQR(a[i][k]);
			sigma=SIGN(sqrt(sum),a[k][k]);
			a[k][k] += sigma;
			c[k]=sigma*a[k][k];
			d[k] = -scale*sigma;
			for (j=k+1;j<n;j++) {
				for (sum=0.0,i=k;i<n;i++) sum += a[i][k]*a[i][j];
				tau=sum/c[k];
				for (i=k;i<n;i++) a[i][j] -= tau*a[i][k];
			}
		}
	}
	d[n-1]=a[n-1][n-1];
	if (d[n-1] == 0.0) sing=true;
}
