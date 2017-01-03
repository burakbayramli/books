#include <cmath>
#include <complex>
#include <limits>
#include "nr.h"
using namespace std;

void NR::cisi(const DP x, complex<DP> &cs)
{
	const int MAXIT=100;
	const DP EULER=0.577215664901533, PIBY2=1.570796326794897, TMIN=2.0;
	const DP EPS=numeric_limits<DP>::epsilon();
	const DP FPMIN=numeric_limits<DP>::min()*4.0;
	const DP BIG=numeric_limits<DP>::max()*EPS;
	int i,k;
	bool odd;
	DP a,err,fact,sign,sum,sumc,sums,t,term;
	complex<DP> h,b,c,d,del;

	t=fabs(x);
	if (t == 0.0) {
		cs= -BIG;
		return;
	}
	if (t > TMIN) {
		b=complex<DP>(1.0,t);
		c=complex<DP>(BIG,0.0);
		d=h=1.0/b;
		for (i=1;i<MAXIT;i++) {
			a= -i*i;
			b += 2.0;
			d=1.0/(a*d+b);
			c=b+a/c;
			del=c*d;
			h *= del;
			if (fabs(real(del)-1.0)+fabs(imag(del)) <= EPS) break;
		}
		if (i >= MAXIT) nrerror("cf failed in cisi");
		h=complex<DP>(cos(t),-sin(t))*h;
		cs= -conj(h)+complex<DP>(0.0,PIBY2);
	} else {
		if (t < sqrt(FPMIN)) {
			sumc=0.0;
			sums=t;
		} else {
			sum=sums=sumc=0.0;
			sign=fact=1.0;
			odd=true;
			for (k=1;k<=MAXIT;k++) {
				fact *= t/k;
				term=fact/k;
				sum += sign*term;
				err=term/fabs(sum);
				if (odd) {
					sign = -sign;
					sums=sum;
					sum=sumc;
				} else {
					sumc=sum;
					sum=sums;
				}
				if (err < EPS) break;
				odd=!odd;
			}
			if (k > MAXIT) nrerror("maxits exceeded in cisi");
		}
		cs=complex<DP>(sumc+log(t)+EULER,sums);
	}
	if (x < 0.0) cs = conj(cs);
}
