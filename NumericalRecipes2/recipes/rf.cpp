#include <cmath>
#include "nr.h"
using namespace std;

DP NR::rf(const DP x, const DP y, const DP z)
{
	const DP ERRTOL=0.0025, TINY=1.5e-38, BIG=3.0e37, THIRD=1.0/3.0;
	const DP C1=1.0/24.0, C2=0.1, C3=3.0/44.0, C4=1.0/14.0;
	DP alamb,ave,delx,dely,delz,e2,e3,sqrtx,sqrty,sqrtz,xt,yt,zt;

	if (MIN(MIN(x,y),z) < 0.0 || MIN(MIN(x+y,x+z),y+z) < TINY ||
		MAX(MAX(x,y),z) > BIG)
			nrerror("invalid arguments in rf");
	xt=x;
	yt=y;
	zt=z;
	do {
		sqrtx=sqrt(xt);
		sqrty=sqrt(yt);
		sqrtz=sqrt(zt);
		alamb=sqrtx*(sqrty+sqrtz)+sqrty*sqrtz;
		xt=0.25*(xt+alamb);
		yt=0.25*(yt+alamb);
		zt=0.25*(zt+alamb);
		ave=THIRD*(xt+yt+zt);
		delx=(ave-xt)/ave;
		dely=(ave-yt)/ave;
		delz=(ave-zt)/ave;
	} while (MAX(MAX(fabs(delx),fabs(dely)),fabs(delz)) > ERRTOL);
	e2=delx*dely-delz*delz;
	e3=delx*dely*delz;
	return (1.0+(C1*e2-C2-C3*e3)*e2+C4*e3)/sqrt(ave);
}
