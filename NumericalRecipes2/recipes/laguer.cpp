#include <cmath>
#include <complex>
#include <limits>
#include "nr.h"
using namespace std;

void NR::laguer(Vec_I_CPLX_DP &a, complex<DP> &x, int &its)
{
	const int MR=8,MT=10,MAXIT=MT*MR;
	const DP EPS=numeric_limits<DP>::epsilon();
	static const DP frac[MR+1]=
		{0.0,0.5,0.25,0.75,0.13,0.38,0.62,0.88,1.0};
	int iter,j;
	DP abx,abp,abm,err;
	complex<DP> dx,x1,b,d,f,g,h,sq,gp,gm,g2;

	int m=a.size()-1;
	for (iter=1;iter<=MAXIT;iter++) {
		its=iter;
		b=a[m];
		err=abs(b);
		d=f=0.0;
		abx=abs(x);
		for (j=m-1;j>=0;j--) {
			f=x*f+d;
			d=x*d+b;
			b=x*b+a[j];
			err=abs(b)+abx*err;
		}
		err *= EPS;
		if (abs(b) <= err) return;
		g=d/b;
		g2=g*g;
		h=g2-2.0*f/b;
		sq=sqrt(DP(m-1)*(DP(m)*h-g2));
		gp=g+sq;
		gm=g-sq;
		abp=abs(gp);
		abm=abs(gm);
		if (abp < abm) gp=gm;
		dx=MAX(abp,abm) > 0.0 ? DP(m)/gp : polar(1+abx,DP(iter));
		x1=x-dx;
		if (x == x1) return;
		if (iter % MT != 0) x=x1;
		else x -= frac[iter/MT]*dx;
	}
	nrerror("too many iterations in laguer");
	return;
}
