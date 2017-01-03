#include <cmath>
#include "nr.h"
using namespace std;

void NR::elmhes(Mat_IO_DP &a)
{
	int i,j,m;
	DP y,x;

	int n=a.nrows();
	for (m=1;m<n-1;m++) {
		x=0.0;
		i=m;
		for (j=m;j<n;j++) {
			if (fabs(a[j][m-1]) > fabs(x)) {
				x=a[j][m-1];
				i=j;
			}
		}
		if (i != m) {
			for (j=m-1;j<n;j++) SWAP(a[i][j],a[m][j]);
			for (j=0;j<n;j++) SWAP(a[j][i],a[j][m]);
		}
		if (x != 0.0) {
			for (i=m+1;i<n;i++) {
				if ((y=a[i][m-1]) != 0.0) {
					y /= x;
					a[i][m-1]=y;
					for (j=m;j<n;j++) a[i][j] -= y*a[m][j];
					for (j=0;j<n;j++) a[j][m] += y*a[j][i];
				}
			}
		}
	}
}
