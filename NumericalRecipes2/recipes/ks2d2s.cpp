#include <cmath>
#include "nr.h"
using namespace std;

void NR::ks2d2s(Vec_I_DP &x1, Vec_I_DP &y1, Vec_I_DP &x2, Vec_I_DP &y2, DP &d,
	DP &prob)
{
	int j;
	DP d1,d2,dum,dumm,fa,fb,fc,fd,ga,gb,gc,gd,r1,r2,rr,sqen;

	int n1=x1.size();
	int n2=x2.size();
	d1=0.0;
	for (j=0;j<n1;j++) {
		quadct(x1[j],y1[j],x1,y1,fa,fb,fc,fd);
		quadct(x1[j],y1[j],x2,y2,ga,gb,gc,gd);
		d1=MAX(d1,fabs(fa-ga));
		d1=MAX(d1,fabs(fb-gb));
		d1=MAX(d1,fabs(fc-gc));
		d1=MAX(d1,fabs(fd-gd));
	}
	d2=0.0;
	for (j=0;j<n2;j++) {
		quadct(x2[j],y2[j],x1,y1,fa,fb,fc,fd);
		quadct(x2[j],y2[j],x2,y2,ga,gb,gc,gd);
		d2=MAX(d2,fabs(fa-ga));
		d2=MAX(d2,fabs(fb-gb));
		d2=MAX(d2,fabs(fc-gc));
		d2=MAX(d2,fabs(fd-gd));
	}
	d=0.5*(d1+d2);
	sqen=sqrt(n1*n2/DP(n1+n2));
	pearsn(x1,y1,r1,dum,dumm);
	pearsn(x2,y2,r2,dum,dumm);
	rr=sqrt(1.0-0.5*(r1*r1+r2*r2));
	prob=probks(d*sqen/(1.0+rr*(0.25-0.75/sqen)));
}
