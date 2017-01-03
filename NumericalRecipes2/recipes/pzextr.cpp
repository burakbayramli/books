#include "nr.h"

extern Vec_DP *x_p;
extern Mat_DP *d_p;

void NR::pzextr(const int iest, const DP xest, Vec_I_DP &yest, Vec_O_DP &yz,
	Vec_O_DP &dy)
{
	int j,k1;
	DP q,f2,f1,delta;

	int nv=yz.size();
	Vec_DP c(nv);
	Vec_DP &x=*x_p;
	Mat_DP &d=*d_p;
	x[iest]=xest;
	for (j=0;j<nv;j++) dy[j]=yz[j]=yest[j];
	if (iest == 0) {
		for (j=0;j<nv;j++) d[j][0]=yest[j];
	} else {
		for (j=0;j<nv;j++) c[j]=yest[j];
		for (k1=0;k1<iest;k1++) {
			delta=1.0/(x[iest-k1-1]-xest);
			f1=xest*delta;
			f2=x[iest-k1-1]*delta;
			for (j=0;j<nv;j++) {
				q=d[j][k1];
				d[j][k1]=dy[j];
				delta=c[j]-q;
				dy[j]=f1*delta;
				c[j]=f2*delta;
				yz[j] += dy[j];
			}
		}
		for (j=0;j<nv;j++) d[j][iest]=dy[j];
	}
}
