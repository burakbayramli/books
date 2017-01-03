#include "nr.h"

DP NR::amotry(Mat_IO_DP &p, Vec_O_DP &y, Vec_IO_DP &psum, DP funk(Vec_I_DP &),
	const int ihi, const DP fac)
{
	int j;
	DP fac1,fac2,ytry;

	int ndim=p.ncols();
	Vec_DP ptry(ndim);
	fac1=(1.0-fac)/ndim;
	fac2=fac1-fac;
	for (j=0;j<ndim;j++)
		ptry[j]=psum[j]*fac1-p[ihi][j]*fac2;
	ytry=funk(ptry);
	if (ytry < y[ihi]) {
		y[ihi]=ytry;
		for (j=0;j<ndim;j++) {
			psum[j] += ptry[j]-p[ihi][j];
			p[ihi][j]=ptry[j];
		}
	}
	return ytry;
}
