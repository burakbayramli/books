#include <cmath>
#include "nr.h"
using namespace std;

namespace {
	DP func(DP funk(const DP), const DP aa, const DP x)
	{
		return 2.0*x*funk(aa+x*x);
	}
}

DP NR::midsql(DP funk(const DP), const DP aa, const DP bb, const int n)
{
	DP x,tnm,sum,del,ddel,a,b;
	static DP s;
	int it,j;

	b=sqrt(bb-aa);
	a=0.0;
	if (n == 1) {
		return (s=(b-a)*func(funk,aa,0.5*(a+b)));
	} else {
		for(it=1,j=1;j<n-1;j++) it *= 3;
		tnm=it;
		del=(b-a)/(3.0*tnm);
		ddel=del+del;
		x=a+0.5*del;
		sum=0.0;
		for (j=0;j<it;j++) {
			sum += func(funk,aa,x);
			x += ddel;
			sum += func(funk,aa,x);
			x += del;
		}
		s=(s+(b-a)*sum/tnm)/3.0;
		return s;
	}
}
