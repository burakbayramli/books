#include "nr.h"

void NR::ddpoly(Vec_I_DP &c, const DP x, Vec_O_DP &pd)
{
	int nnd,j,i;
	DP cnst=1.0;

	int nc=c.size()-1;
	int nd=pd.size()-1;
	pd[0]=c[nc];
	for (j=1;j<nd+1;j++) pd[j]=0.0;
	for (i=nc-1;i>=0;i--) {
		nnd=(nd < (nc-i) ? nd : nc-i);
		for (j=nnd;j>0;j--)
			pd[j]=pd[j]*x+pd[j-1];
		pd[0]=pd[0]*x+c[i];
	}
	for (i=2;i<nd+1;i++) {
		cnst *= i;
		pd[i] *= cnst;
	}
}
