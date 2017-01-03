#include <cmath>
#include "nr.h"
using namespace std;

namespace {
	inline void get_psum(Mat_I_DP &p, Vec_O_DP &psum)
	{
		int i,j;
		DP sum;

		int mpts=p.nrows();
		int ndim=p.ncols();
		for (j=0;j<ndim;j++) {
			for (sum=0.0,i=0;i<mpts;i++)
				sum += p[i][j];
			psum[j]=sum;
		}
	}
}

void NR::amoeba(Mat_IO_DP &p, Vec_IO_DP &y, const DP ftol, DP funk(Vec_I_DP &),
	int &nfunk)
{
	const int NMAX=5000;
	const DP TINY=1.0e-10;
	int i,ihi,ilo,inhi,j;
	DP rtol,ysave,ytry;

	int mpts=p.nrows();
	int ndim=p.ncols();
	Vec_DP psum(ndim);
	nfunk=0;
	get_psum(p,psum);
	for (;;) {
		ilo=0;
		ihi = y[0]>y[1] ? (inhi=1,0) : (inhi=0,1);
		for (i=0;i<mpts;i++) {
			if (y[i] <= y[ilo]) ilo=i;
			if (y[i] > y[ihi]) {
				inhi=ihi;
				ihi=i;
			} else if (y[i] > y[inhi] && i != ihi) inhi=i;
		}
		rtol=2.0*fabs(y[ihi]-y[ilo])/(fabs(y[ihi])+fabs(y[ilo])+TINY);
		if (rtol < ftol) {
			SWAP(y[0],y[ilo]);
			for (i=0;i<ndim;i++) SWAP(p[0][i],p[ilo][i]);
			break;
		}
		if (nfunk >= NMAX) nrerror("NMAX exceeded");
		nfunk += 2;
		ytry=amotry(p,y,psum,funk,ihi,-1.0);
		if (ytry <= y[ilo])
			ytry=amotry(p,y,psum,funk,ihi,2.0);
		else if (ytry >= y[inhi]) {
			ysave=y[ihi];
			ytry=amotry(p,y,psum,funk,ihi,0.5);
			if (ytry >= ysave) {
				for (i=0;i<mpts;i++) {
					if (i != ilo) {
						for (j=0;j<ndim;j++)
							p[i][j]=psum[j]=0.5*(p[i][j]+p[ilo][j]);
						y[i]=funk(psum);
					}
				}
				nfunk += ndim;
				get_psum(p,psum);
			}
		} else --nfunk;
	}
}
