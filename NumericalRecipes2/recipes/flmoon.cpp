#include <cmath>
#include "nr.h"
using namespace std;

void NR::flmoon(const int n, const int nph, int &jd, DP &frac)
{
	const DP RAD=3.141592653589793238/180.0;
	int i;
	DP am,as,c,t,t2,xtra;

	c=n+nph/4.0;
	t=c/1236.85;
	t2=t*t;
	as=359.2242+29.105356*c;
	am=306.0253+385.816918*c+0.010730*t2;
	jd=2415020+28*n+7*nph;
	xtra=0.75933+1.53058868*c+((1.178e-4)-(1.55e-7)*t)*t2;
	if (nph == 0 || nph == 2)
		xtra += (0.1734-3.93e-4*t)*sin(RAD*as)-0.4068*sin(RAD*am);
	else if (nph == 1 || nph == 3)
		xtra += (0.1721-4.0e-4*t)*sin(RAD*as)-0.6280*sin(RAD*am);
	else nrerror("nph is unknown in flmoon");
	i=int(xtra >= 0.0 ? floor(xtra) : ceil(xtra-1.0));
	jd += i;
	frac=xtra-i;
}
