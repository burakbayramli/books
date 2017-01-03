#include <cmath>
#include "nr.h"
using namespace std;

extern DP x;

void NR::kermom(Vec_O_DP &w, const DP y)
{
	DP d,df,clog,x2,x3,x4,y2;

	int m=w.size();
	if (y >= x) {
		d=y-x;
		df=2.0*sqrt(d)*d;
		w[0]=df/3.0;
		w[1]=df*(x/3.0+d/5.0);
		w[2]=df*((x/3.0 + 0.4*d)*x + d*d/7.0);
		w[3]=df*(((x/3.0 + 0.6*d)*x + 3.0*d*d/7.0)*x+d*d*d/9.0);
	} else {
		x3=(x2=x*x)*x;
		x4=x2*x2;
		y2=y*y;
		d=x-y;
		w[0]=d*((clog=log(d))-1.0);
		w[1] = -0.25*(3.0*x+y-2.0*clog*(x+y))*d;
		w[2]=(-11.0*x3+y*(6.0*x2+y*(3.0*x+2.0*y))
			+6.0*clog*(x3-y*y2))/18.0;
		w[3]=(-25.0*x4+y*(12.0*x3+y*(6.0*x2+y*
			(4.0*x+3.0*y)))+12.0*clog*(x4-(y2*y2)))/48.0;
	}
}
