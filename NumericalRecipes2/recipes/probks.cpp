#include <cmath>
#include "nr.h"
using namespace std;

DP NR::probks(const DP alam)
{
	const DP EPS1=1.0e-6,EPS2=1.0e-16;
	int j;
	DP a2,fac=2.0,sum=0.0,term,termbf=0.0;

	a2 = -2.0*alam*alam;
	for (j=1;j<=100;j++) {
		term=fac*exp(a2*j*j);
		sum += term;
		if (fabs(term) <= EPS1*termbf || fabs(term) <= EPS2*sum) return sum;
		fac = -fac;
		termbf=fabs(term);
	}
	return 1.0;
}
