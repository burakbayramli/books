#include <cmath>
#include "nr.h"
using namespace std;

DP NR::qsimp(DP func(const DP), const DP a, const DP b)
{
	const int JMAX=20;
	const DP EPS=1.0e-10;
	int j;
	DP s,st,ost=0.0,os=0.0;

	for (j=0;j<JMAX;j++) {
		st=trapzd(func,a,b,j+1);
		s=(4.0*st-ost)/3.0;
		if (j > 5)
			if (fabs(s-os) < EPS*fabs(os) ||
				(s == 0.0 && os == 0.0)) return s;
		os=s;
		ost=st;
	}
	nrerror("Too many steps in routine qsimp");
	return 0.0;
}
