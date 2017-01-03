#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

void NR::lnsrch(Vec_I_DP &xold, const DP fold, Vec_I_DP &g, Vec_IO_DP &p,
	Vec_O_DP &x, DP &f, const DP stpmax, bool &check, DP func(Vec_I_DP &))
{
	const DP ALF=1.0e-4, TOLX=numeric_limits<DP>::epsilon();
	int i;
	DP a,alam,alam2=0.0,alamin,b,disc,f2=0.0;
	DP rhs1,rhs2,slope,sum,temp,test,tmplam;

	int n=xold.size();
	check=false;
	sum=0.0;
	for (i=0;i<n;i++) sum += p[i]*p[i];
	sum=sqrt(sum);
	if (sum > stpmax)
		for (i=0;i<n;i++) p[i] *= stpmax/sum;
	slope=0.0;
	for (i=0;i<n;i++)
		slope += g[i]*p[i];
	if (slope >= 0.0) nrerror("Roundoff problem in lnsrch.");
	test=0.0;
	for (i=0;i<n;i++) {
		temp=fabs(p[i])/MAX(fabs(xold[i]),1.0);
		if (temp > test) test=temp;
	}
	alamin=TOLX/test;
	alam=1.0;
	for (;;) {
		for (i=0;i<n;i++) x[i]=xold[i]+alam*p[i];
		f=func(x);
		if (alam < alamin) {
			for (i=0;i<n;i++) x[i]=xold[i];
			check=true;
			return;
		} else if (f <= fold+ALF*alam*slope) return;
		else {
			if (alam == 1.0)
				tmplam = -slope/(2.0*(f-fold-slope));
			else {
				rhs1=f-fold-alam*slope;
				rhs2=f2-fold-alam2*slope;
				a=(rhs1/(alam*alam)-rhs2/(alam2*alam2))/(alam-alam2);
				b=(-alam2*rhs1/(alam*alam)+alam*rhs2/(alam2*alam2))/(alam-alam2);
				if (a == 0.0) tmplam = -slope/(2.0*b);
				else {
					disc=b*b-3.0*a*slope;
					if (disc < 0.0) tmplam=0.5*alam;
					else if (b <= 0.0) tmplam=(-b+sqrt(disc))/(3.0*a);
					else tmplam=-slope/(b+sqrt(disc));
				}
				if (tmplam>0.5*alam)
					tmplam=0.5*alam;
			}
		}
		alam2=alam;
		f2 = f;
		alam=MAX(tmplam,0.1*alam);
	}
}
