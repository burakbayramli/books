#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

void NR::gcf(DP &gammcf, const DP a, const DP x, DP &gln)
{
	const int ITMAX=100;
	const DP EPS=numeric_limits<DP>::epsilon();
	const DP FPMIN=numeric_limits<DP>::min()/EPS;
	int i;
	DP an,b,c,d,del,h;

	gln=gammln(a);
	b=x+1.0-a;
	c=1.0/FPMIN;
	d=1.0/b;
	h=d;
	for (i=1;i<=ITMAX;i++) {
		an = -i*(i-a);
		b += 2.0;
		d=an*d+b;
		if (fabs(d) < FPMIN) d=FPMIN;
		c=b+an/c;
		if (fabs(c) < FPMIN) c=FPMIN;
		d=1.0/d;
		del=d*c;
		h *= del;
		if (fabs(del-1.0) <= EPS) break;
	}
	if (i > ITMAX) nrerror("a too large, ITMAX too small in gcf");
	gammcf=exp(-x+a*log(x)-gln)*h;
}
