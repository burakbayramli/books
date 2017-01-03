#include <cmath>
#include "nr.h"
using namespace std;

void NR::pearsn(Vec_I_DP &x, Vec_I_DP &y, DP &r, DP &prob, DP &z)
{
	const DP TINY=1.0e-20;
	int j;
	DP yt,xt,t,df;
	DP syy=0.0,sxy=0.0,sxx=0.0,ay=0.0,ax=0.0;

	int n=x.size();
	for (j=0;j<n;j++) {
		ax += x[j];
		ay += y[j];
	}
	ax /= n;
	ay /= n;
	for (j=0;j<n;j++) {
		xt=x[j]-ax;
		yt=y[j]-ay;
		sxx += xt*xt;
		syy += yt*yt;
		sxy += xt*yt;
	}
	r=sxy/(sqrt(sxx*syy)+TINY);
	z=0.5*log((1.0+r+TINY)/(1.0-r+TINY));
	df=n-2;
	t=r*sqrt(df/((1.0-r+TINY)*(1.0+r+TINY)));
	prob=betai(0.5*df,0.5,df/(df+t*t));
	// prob=erfcc(fabs(z*sqrt(n-1.0))/1.4142136);
}
