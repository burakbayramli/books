#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

void NR::balanc(Mat_IO_DP &a)
{
	const DP RADIX = numeric_limits<DP>::radix;
	int i,j,last=0;
	DP s,r,g,f,c,sqrdx;

	int n=a.nrows();
	sqrdx=RADIX*RADIX;
	while (last == 0) {
		last=1;
		for (i=0;i<n;i++) {
			r=c=0.0;
			for (j=0;j<n;j++)
				if (j != i) {
					c += fabs(a[j][i]);
					r += fabs(a[i][j]);
				}
			if (c != 0.0 && r != 0.0) {
				g=r/RADIX;
				f=1.0;
				s=c+r;
				while (c<g) {
					f *= RADIX;
					c *= sqrdx;
				}
				g=r*RADIX;
				while (c>g) {
					f /= RADIX;
					c /= sqrdx;
				}
				if ((c+r)/f < 0.95*s) {
					last=0;
					g=1.0/f;
					for (j=0;j<n;j++) a[i][j] *= g;
					for (j=0;j<n;j++) a[j][i] *= f;
				}
			}
		}
	}
}
