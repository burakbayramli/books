#include <cmath>
#include "nr.h"
using namespace std;

namespace {
	inline void get_psum(Mat_I_DP &p, Vec_O_DP &psum)
	{
		int n,m;
		DP sum;

		int mpts=p.nrows();
		int ndim=p.ncols();
		for (n=0;n<ndim;n++) {
			for (sum=0.0,m=0;m<mpts;m++) sum += p[m][n];
			psum[n]=sum;
		}
	}
}

extern int idum;
DP tt;

void NR::amebsa(Mat_IO_DP &p, Vec_IO_DP &y, Vec_O_DP &pb, DP &yb, const DP ftol,
	DP funk(Vec_I_DP &), int &iter, const DP temptr)
{
	int i,ihi,ilo,j,n;
	DP rtol,yhi,ylo,ynhi,ysave,yt,ytry;

	int mpts=p.nrows();
	int ndim=p.ncols();
	Vec_DP psum(ndim);
	tt = -temptr;
	get_psum(p,psum);
	for (;;) {
		ilo=0;
		ihi=1;
		ynhi=ylo=y[0]+tt*log(ran1(idum));
		yhi=y[1]+tt*log(ran1(idum));
		if (ylo > yhi) {
			ihi=0;
			ilo=1;
			ynhi=yhi;
			yhi=ylo;
			ylo=ynhi;
		}
		for (i=3;i<=mpts;i++) {
			yt=y[i-1]+tt*log(ran1(idum));
			if (yt <= ylo) {
				ilo=i-1;
				ylo=yt;
			}
			if (yt > yhi) {
				ynhi=yhi;
				ihi=i-1;
				yhi=yt;
			} else if (yt > ynhi) {
				ynhi=yt;
			}
		}
		rtol=2.0*fabs(yhi-ylo)/(fabs(yhi)+fabs(ylo));
		if (rtol < ftol || iter < 0) {
			SWAP(y[0],y[ilo]);
			for (n=0;n<ndim;n++)
				SWAP(p[0][n],p[ilo][n]);
			break;
		}
		iter -= 2;
		ytry=amotsa(p,y,psum,pb,yb,funk,ihi,yhi,-1.0);
		if (ytry <= ylo) {
			ytry=amotsa(p,y,psum,pb,yb,funk,ihi,yhi,2.0);
		} else if (ytry >= ynhi) {
			ysave=yhi;
			ytry=amotsa(p,y,psum,pb,yb,funk,ihi,yhi,0.5);
			if (ytry >= ysave) {
				for (i=0;i<mpts;i++) {
					if (i != ilo) {
						for (j=0;j<ndim;j++) {
							psum[j]=0.5*(p[i][j]+p[ilo][j]);
							p[i][j]=psum[j];
						}
						y[i]=funk(psum);
					}
				}
				iter -= ndim;
				get_psum(p,psum);
			}
		} else ++iter;
	}
}
