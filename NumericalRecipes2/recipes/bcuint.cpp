#include "nr.h"

void NR::bcuint(Vec_I_DP &y, Vec_I_DP &y1, Vec_I_DP &y2, Vec_I_DP &y12,
	const DP x1l, const DP x1u, const DP x2l, const DP x2u,
	const DP x1, const DP x2, DP &ansy, DP &ansy1, DP &ansy2)
{
	int i;
	DP t,u,d1,d2;
	Mat_DP c(4,4);

	d1=x1u-x1l;
	d2=x2u-x2l;
	bcucof(y,y1,y2,y12,d1,d2,c);
	if (x1u == x1l || x2u == x2l)
		nrerror("Bad input in routine bcuint");
	t=(x1-x1l)/d1;
	u=(x2-x2l)/d2;
	ansy=ansy2=ansy1=0.0;
	for (i=3;i>=0;i--) {
		ansy=t*ansy+((c[i][3]*u+c[i][2])*u+c[i][1])*u+c[i][0];
		ansy2=t*ansy2+(3.0*c[i][3]*u+2.0*c[i][2])*u+c[i][1];
		ansy1=u*ansy1+(3.0*c[3][i]*t+2.0*c[2][i])*t+c[1][i];
	}
	ansy1 /= d1;
	ansy2 /= d2;
}
