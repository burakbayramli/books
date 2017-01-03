#include "nr.h"

DP NR::midpnt(DP func(const DP), const DP a, const DP b, const int n)
{
	int it,j;
	DP x,tnm,sum,del,ddel;
	static DP s;

	if (n == 1) {
		return (s=(b-a)*func(0.5*(a+b)));
	} else {
		for(it=1,j=1;j<n-1;j++) it *= 3;
		tnm=it;
		del=(b-a)/(3.0*tnm);
		ddel=del+del;
		x=a+0.5*del;
		sum=0.0;
		for (j=0;j<it;j++) {
			sum += func(x);
			x += ddel;
			sum += func(x);
			x += del;
		}
		s=(s+(b-a)*sum/tnm)/3.0;
		return s;
	}
}
