#include <cmath>
#include "nr.h"
using namespace std;

void NR::cntab2(Mat_I_INT &nn, DP &h, DP &hx, DP &hy, DP &hygx, DP &hxgy,
	DP &uygx, DP &uxgy, DP &uxy)
{
	const DP TINY=1.0e-30;
	int i,j;
	DP sum=0.0,p;

	int ni=nn.nrows();
	int nj=nn.ncols();
	Vec_DP sumi(ni),sumj(nj);
	for (i=0;i<ni;i++) {
		sumi[i]=0.0;
		for (j=0;j<nj;j++) {
			sumi[i] += nn[i][j];
			sum += nn[i][j];
		}
	}
	for (j=0;j<nj;j++) {
		sumj[j]=0.0;
		for (i=0;i<ni;i++)
			sumj[j] += nn[i][j];
	}
	hx=0.0;
	for (i=0;i<ni;i++)
		if (sumi[i] != 0.0) {
			p=sumi[i]/sum;
			hx -= p*log(p);
		}
	hy=0.0;
	for (j=0;j<nj;j++)
		if (sumj[j] != 0.0) {
			p=sumj[j]/sum;
			hy -= p*log(p);
		}
	h=0.0;
	for (i=0;i<ni;i++)
		for (j=0;j<nj;j++)
			if (nn[i][j] != 0) {
				p=nn[i][j]/sum;
				h -= p*log(p);
			}
	hygx=h-hx;
	hxgy=h-hy;
	uygx=(hy-hygx)/(hy+TINY);
	uxgy=(hx-hxgy)/(hx+TINY);
	uxy=2.0*(hx+hy-h)/(hx+hy+TINY);
}
