#include <cmath>
#include "nr.h"
using namespace std;

void NR::sphbes(const int n, const DP x, DP &sj, DP &sy, DP &sjp, DP &syp)
{
	const DP RTPIO2=1.253314137315500251;
	DP factor,order,rj,rjp,ry,ryp;

	if (n < 0 || x <= 0.0) nrerror("bad arguments in sphbes");
	order=n+0.5;
	bessjy(x,order,rj,ry,rjp,ryp);
	factor=RTPIO2/sqrt(x);
	sj=factor*rj;
	sy=factor*ry;
	sjp=factor*rjp-sj/(2.0*x);
	syp=factor*ryp-sy/(2.0*x);
}
