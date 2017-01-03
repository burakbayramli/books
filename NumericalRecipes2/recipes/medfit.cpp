#include <cmath>
#include "nr.h"
using namespace std;

DP aa,abdevt;
const Vec_DP *xt_p,*yt_p;

void NR::medfit(Vec_I_DP &x, Vec_I_DP &y, DP &a, DP &b, DP &abdev)
{
	int j;
	DP bb,b1,b2,del,f,f1,f2,sigb,temp;
	DP sx=0.0,sy=0.0,sxy=0.0,sxx=0.0,chisq=0.0;

	int ndata=x.size();
	xt_p= &x;
	yt_p= &y;
	for (j=0;j<ndata;j++) {
		sx += x[j];
		sy += y[j];
		sxy += x[j]*y[j];
		sxx += x[j]*x[j];
	}
	del=ndata*sxx-sx*sx;
	aa=(sxx*sy-sx*sxy)/del;
	bb=(ndata*sxy-sx*sy)/del;
	for (j=0;j<ndata;j++)
		chisq += (temp=y[j]-(aa+bb*x[j]),temp*temp);
	sigb=sqrt(chisq/del);
	b1=bb;
	f1=rofunc(b1);
	if (sigb > 0.0) {
		b2=bb+SIGN(3.0*sigb,f1);
		f2=rofunc(b2);
		if (b2 == b1) {
			a=aa;
			b=bb;
			abdev=abdevt/ndata;
			return;
		}
		while (f1*f2 > 0.0) {
			bb=b2+1.6*(b2-b1);
			b1=b2;
			f1=f2;
			b2=bb;
			f2=rofunc(b2);
		}
		sigb=0.01*sigb;
		while (fabs(b2-b1) > sigb) {
			bb=b1+0.5*(b2-b1);
			if (bb == b1 || bb == b2) break;
			f=rofunc(bb);
			if (f*f1 >= 0.0) {
				f1=f;
				b1=bb;
			} else {
				f2=f;
				b2=bb;
			}
		}
	}
	a=aa;
	b=bb;
	abdev=abdevt/ndata;
}
