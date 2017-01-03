#include "nr.h"

extern Vec_DP *x_p;
extern Mat_DP *d_p;

void NR::rzextr(const int iest, const DP xest, Vec_I_DP &yest, Vec_O_DP &yz,
	Vec_O_DP &dy)
{
	int j,k,nv;
	DP yy,v,ddy,c,b1,b;

	nv=yz.size();
	Vec_DP fx(iest+1);
	Vec_DP &x=*x_p;
	Mat_DP &d=*d_p;
	x[iest]=xest;
	if (iest == 0)
		for (j=0;j<nv;j++) {
			yz[j]=yest[j];
			d[j][0]=yest[j];
			dy[j]=yest[j];
		}
	else {
		for (k=0;k<iest;k++)
			fx[k+1]=x[iest-(k+1)]/xest;
		for (j=0;j<nv;j++) {
			v=d[j][0];
			d[j][0]=yy=c=yest[j];
			for (k=1;k<=iest;k++) {
				b1=fx[k]*v;
				b=b1-c;
				if (b != 0.0) {
					b=(c-v)/b;
					ddy=c*b;
					c=b1*b;
				} else
					ddy=v;
				if (k != iest) v=d[j][k];
				d[j][k]=ddy;
				yy += ddy;
			}
			dy[j]=ddy;
			yz[j]=yy;
		}
	}
}
