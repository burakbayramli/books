#include <cmath>
#include "nr.h"
using namespace std;

void NR::fit(Vec_I_DP &x, Vec_I_DP &y, Vec_I_DP &sig, const bool mwt, DP &a,
	DP &b, DP &siga, DP &sigb, DP &chi2, DP &q)
{
	int i;
	DP wt,t,sxoss,sx=0.0,sy=0.0,st2=0.0,ss,sigdat;

	int ndata=x.size();
	b=0.0;
	if (mwt) {
		ss=0.0;
		for (i=0;i<ndata;i++) {
			wt=1.0/SQR(sig[i]);
			ss += wt;
			sx += x[i]*wt;
			sy += y[i]*wt;
		}
	} else {
		for (i=0;i<ndata;i++) {
			sx += x[i];
			sy += y[i];
		}
		ss=ndata;
	}
	sxoss=sx/ss;
	if (mwt) {
		for (i=0;i<ndata;i++) {
			t=(x[i]-sxoss)/sig[i];
			st2 += t*t;
			b += t*y[i]/sig[i];
		}
	} else {
		for (i=0;i<ndata;i++) {
			t=x[i]-sxoss;
			st2 += t*t;
			b += t*y[i];
		}
	}
	b /= st2;
	a=(sy-sx*b)/ss;
	siga=sqrt((1.0+sx*sx/(ss*st2))/ss);
	sigb=sqrt(1.0/st2);
	chi2=0.0;
	q=1.0;
	if (!mwt) {
		for (i=0;i<ndata;i++)
			chi2 += SQR(y[i]-a-b*x[i]);
		sigdat=sqrt(chi2/(ndata-2));
		siga *= sigdat;
		sigb *= sigdat;
	} else {
		for (i=0;i<ndata;i++)
			chi2 += SQR((y[i]-a-b*x[i])/sig[i]);
		if (ndata>2) q=gammq(0.5*(ndata-2),0.5*chi2);
	}
}
