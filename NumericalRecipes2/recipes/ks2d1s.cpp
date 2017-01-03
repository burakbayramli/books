#include <cmath>
#include "nr.h"
using namespace std;

void NR::ks2d1s(Vec_I_DP &x1, Vec_I_DP &y1, void quadvl(const DP, const DP,
	DP &, DP &, DP &, DP &), DP &d1, DP &prob)
{
	int j;
	DP dum,dumm,fa,fb,fc,fd,ga,gb,gc,gd,r1,rr,sqen;

	int n1=x1.size();
	d1=0.0;
	for (j=0;j<n1;j++) {
		quadct(x1[j],y1[j],x1,y1,fa,fb,fc,fd);
		quadvl(x1[j],y1[j],ga,gb,gc,gd);
		d1=MAX(d1,fabs(fa-ga));
		d1=MAX(d1,fabs(fb-gb));
		d1=MAX(d1,fabs(fc-gc));
		d1=MAX(d1,fabs(fd-gd));
	}
	pearsn(x1,y1,r1,dum,dumm);
	sqen=sqrt(DP(n1));
	rr=sqrt(1.0-r1*r1);
	prob=probks(d1*sqen/(1.0+rr*(0.25-0.75/sqen)));
}
