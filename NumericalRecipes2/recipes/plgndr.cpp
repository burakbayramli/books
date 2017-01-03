#include <cmath>
#include "nr.h"
using namespace std;

DP NR::plgndr(const int l, const int m, const DP x)
{
	int i,ll;
	DP fact,pll,pmm,pmmp1,somx2;

	if (m < 0 || m > l || fabs(x) > 1.0)
		nrerror("Bad arguments in routine plgndr");
	pmm=1.0;
	if (m > 0) {
		somx2=sqrt((1.0-x)*(1.0+x));
		fact=1.0;
		for (i=1;i<=m;i++) {
			pmm *= -fact*somx2;
			fact += 2.0;
		}
	}
	if (l == m)
		return pmm;
	else {
		pmmp1=x*(2*m+1)*pmm;
		if (l == (m+1))
			return pmmp1;
		else {
			for (ll=m+2;ll<=l;ll++) {
				pll=(x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m);
				pmm=pmmp1;
				pmmp1=pll;
			}
			return pll;
		}
	}
}
