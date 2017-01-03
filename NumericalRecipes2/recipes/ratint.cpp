#include <cmath>
#include "nr.h"
using namespace std;

void NR::ratint(Vec_I_DP &xa, Vec_I_DP &ya, const DP x, DP &y, DP &dy)
{
	const DP TINY=1.0e-25;
	int m,i,ns=0;
	DP w,t,hh,h,dd;

	int n=xa.size();
	Vec_DP c(n),d(n);
	hh=fabs(x-xa[0]);
	for (i=0;i<n;i++) {
		h=fabs(x-xa[i]);
		if (h == 0.0) {
			y=ya[i];
			dy=0.0;
			return;
		} else if (h < hh) {
			ns=i;
			hh=h;
		}
		c[i]=ya[i];
		d[i]=ya[i]+TINY;
	}
	y=ya[ns--];
	for (m=1;m<n;m++) {
		for (i=0;i<n-m;i++) {
			w=c[i+1]-d[i];
			h=xa[i+m]-x;
			t=(xa[i]-x)*d[i]/h;
			dd=t-c[i+1];
			if (dd == 0.0) nrerror("Error in routine ratint");
			dd=w/dd;
			d[i]=c[i+1]*dd;
			c[i]=t*dd;
		}
		y += (dy=(2*(ns+1) < (n-m) ? c[ns+1] : d[ns--]));
	}
}
