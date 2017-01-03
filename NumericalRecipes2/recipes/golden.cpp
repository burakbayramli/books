#include <cmath>
#include "nr.h"
using namespace std;

namespace {
	inline void shft2(DP &a, DP &b, const DP c)
	{
		a=b;
		b=c;
	}

	inline void shft3(DP &a, DP &b, DP &c, const DP d)
	{
		a=b;
		b=c;
		c=d;
	}
}

DP NR::golden(const DP ax, const DP bx, const DP cx, DP f(const DP),
	const DP tol, DP &xmin)
{
	const DP R=0.61803399,C=1.0-R;
	DP f1,f2,x0,x1,x2,x3;

	x0=ax;
	x3=cx;
	if (fabs(cx-bx) > fabs(bx-ax)) {
		x1=bx;
		x2=bx+C*(cx-bx);
	} else {
		x2=bx;
		x1=bx-C*(bx-ax);
	}
	f1=f(x1);
	f2=f(x2);
	while (fabs(x3-x0) > tol*(fabs(x1)+fabs(x2))) {
		if (f2 < f1) {
			shft3(x0,x1,x2,R*x2+C*x3);
			shft2(f1,f2,f(x2));
		} else {
			shft3(x3,x2,x1,R*x1+C*x0);
			shft2(f2,f1,f(x1));
		}
	}
	if (f1 < f2) {
		xmin=x1;
		return f1;
	} else {
		xmin=x2;
		return f2;
	}
}
