#include <cmath>
#include "nr.h"
using namespace std;

extern int idum;
extern DP tt;

DP NR::amotsa(Mat_IO_DP &p, Vec_O_DP &y, Vec_IO_DP &psum, Vec_O_DP &pb, DP &yb,
	DP funk(Vec_I_DP &), const int ihi, DP &yhi, const DP fac)
{
	int j;
	DP fac1,fac2,yflu,ytry;

	int ndim=p.ncols();
	Vec_DP ptry(ndim);
	fac1=(1.0-fac)/ndim;
	fac2=fac1-fac;
	for (j=0;j<ndim;j++)
		ptry[j]=psum[j]*fac1-p[ihi][j]*fac2;
	ytry=funk(ptry);
	if (ytry <= yb) {
		for (j=0;j<ndim;j++) pb[j]=ptry[j];
		yb=ytry;
	}
	yflu=ytry-tt*log(ran1(idum));
	if (yflu < yhi) {
		y[ihi]=ytry;
		yhi=yflu;
		for (j=0;j<ndim;j++) {
			psum[j] += ptry[j]-p[ihi][j];
			p[ihi][j]=ptry[j];
		}
	}
	return yflu;
}
