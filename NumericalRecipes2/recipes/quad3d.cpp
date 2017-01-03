#include "nr.h"

extern DP yy1(const DP),yy2(const DP);
extern DP z1(const DP, const DP);
extern DP z2(const DP, const DP);

namespace NRquad3d {
	DP xsav,ysav;
	DP (*nrfunc)(const DP, const DP, const DP);

	DP f3(const DP z)
	{
		return nrfunc(xsav,ysav,z);
	}

	DP f2(const DP y)
	{
		ysav=y;
		return NR::qgaus(f3,z1(xsav,y),z2(xsav,y));
	}

	DP f1(const DP x)
	{
		xsav=x;
		return NR::qgaus(f2,yy1(x),yy2(x));
	}
}

DP NR::quad3d(DP func(const DP, const DP, const DP), const DP x1, const DP x2)
{
	NRquad3d::nrfunc=func;
	return qgaus(NRquad3d::f1,x1,x2);
}
