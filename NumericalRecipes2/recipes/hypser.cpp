#include <complex>
#include "nr.h"
using namespace std;

void NR::hypser(const complex<DP> &a, const complex<DP> &b,
	const complex<DP> &c, const complex<DP> &z,
	complex<DP> &series, complex<DP> &deriv)
{
	int n;
	complex<DP> aa,bb,cc,fac,temp;

	deriv=0.0;
	fac=1.0;
	temp=fac;
	aa=a;
	bb=b;
	cc=c;
	for (n=1;n<=1000;n++) {
		fac *= ((aa*bb)/cc);
		deriv += fac;
		fac *= ((1.0/n)*z);
		series=temp+fac;
		if (series == temp) return;
		temp=series;
		aa += 1.0;
		bb += 1.0;
		cc += 1.0;
	}
	nrerror("convergence failure in hypser");
}
