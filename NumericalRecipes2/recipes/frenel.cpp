#include <cmath>
#include <complex>
#include <limits>
#include "nr.h"
using namespace std;

void NR::frenel(const DP x, complex<DP> &cs)
{
	const int MAXIT=100;
	const DP EPS=numeric_limits<DP>::epsilon();
	const DP FPMIN=numeric_limits<DP>::min();
	const DP BIG=numeric_limits<DP>::max()*EPS;
	const DP PI=3.141592653589793238, PIBY2=(PI/2.0), XMIN=1.5;
	bool odd;
	int k,n;
	DP a,ax,fact,pix2,sign,sum,sumc,sums,term,test;
	complex<DP> b,cc,d,h,del;

	ax=fabs(x);
	if (ax < sqrt(FPMIN)) {
		cs=ax;
	} else if (ax <= XMIN) {
		sum=sums=0.0;
		sumc=ax;
		sign=1.0;
		fact=PIBY2*ax*ax;
		odd=true;
		term=ax;
		n=3;
		for (k=1;k<=MAXIT;k++) {
			term *= fact/k;
			sum += sign*term/n;
			test=fabs(sum)*EPS;
			if (odd) {
				sign = -sign;
				sums=sum;
				sum=sumc;
			} else {
				sumc=sum;
				sum=sums;
			}
			if (term < test) break;
			odd=!odd;
			n += 2;
		}
		if (k > MAXIT) nrerror("series failed in frenel");
		cs=complex<DP>(sumc,sums);
	} else {
		pix2=PI*ax*ax;
		b=complex<DP>(1.0,-pix2);
		cc=BIG;
		d=h=1.0/b;
		n = -1;
		for (k=2;k<=MAXIT;k++) {
			n += 2;
			a = -n*(n+1);
			b += 4.0;
			d=1.0/(a*d+b);
			cc=b+a/cc;
			del=cc*d;
			h *= del;
			if (fabs(real(del)-1.0)+fabs(imag(del)) <= EPS) break;
		}
		if (k > MAXIT) nrerror("cf failed in frenel");
		h *= complex<DP>(ax,-ax);
		cs=complex<DP>(0.5,0.5)
			*(1.0-complex<DP>(cos(0.5*pix2),sin(0.5*pix2))*h);
	}
	if (x < 0.0) {
		cs = -cs;
	}
	return;
}
