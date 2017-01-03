#include <cmath>
#include "nr.h"
using namespace std;

DP NR::rc(const DP x, const DP y)
{
	const DP ERRTOL=0.0012, TINY=1.69e-38, SQRTNY=1.3e-19, BIG=3.0e37;
	const DP TNBG=TINY*BIG, COMP1=2.236/SQRTNY, COMP2=TNBG*TNBG/25.0;
	const DP THIRD=1.0/3.0, C1=0.3, C2=1.0/7.0, C3=0.375, C4=9.0/22.0;
	DP alamb,ave,s,w,xt,yt;

	if (x < 0.0 || y == 0.0 || (x+fabs(y)) < TINY || (x+fabs(y)) > BIG ||
		(y<-COMP1 && x > 0.0 && x < COMP2))
			nrerror("invalid arguments in rc");
	if (y > 0.0) {
		xt=x;
		yt=y;
		w=1.0;
	} else {
		xt=x-y;
		yt= -y;
		w=sqrt(x)/sqrt(xt);
	}
	do {
		alamb=2.0*sqrt(xt)*sqrt(yt)+yt;
		xt=0.25*(xt+alamb);
		yt=0.25*(yt+alamb);
		ave=THIRD*(xt+yt+yt);
		s=(yt-ave)/ave;
	} while (fabs(s) > ERRTOL);
	return w*(1.0+s*s*(C1+s*(C2+s*(C3+s*C4))))/sqrt(ave);
}
