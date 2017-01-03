#include <cmath>
#include "nr.h"
using namespace std;

void NR::chebft(const DP a, const DP b, Vec_O_DP &c, DP func(const DP))
{
	const DP PI=3.141592653589793;
	int k,j;
	DP fac,bpa,bma,y,sum;

	int n=c.size();
	Vec_DP f(n);
	bma=0.5*(b-a);
	bpa=0.5*(b+a);
	for (k=0;k<n;k++) {
		y=cos(PI*(k+0.5)/n);
		f[k]=func(y*bma+bpa);
	}
	fac=2.0/n;
	for (j=0;j<n;j++) {
		sum=0.0;
		for (k=0;k<n;k++)
			sum += f[k]*cos(PI*j*(k+0.5)/n);
		c[j]=fac*sum;
	}
}
