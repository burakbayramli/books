#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

DP NR::dfridr(DP func(const DP), const DP x, const DP h, DP &err)
{
	const int NTAB=10;
	const DP CON=1.4, CON2=(CON*CON);
	const DP BIG=numeric_limits<DP>::max();
	const DP SAFE=2.0;
	int i,j;
	DP errt,fac,hh,ans;
	Mat_DP a(NTAB,NTAB);

	if (h == 0.0) nrerror("h must be nonzero in dfridr.");
	hh=h;
	a[0][0]=(func(x+hh)-func(x-hh))/(2.0*hh);
	err=BIG;
	for (i=1;i<NTAB;i++) {
		hh /= CON;
		a[0][i]=(func(x+hh)-func(x-hh))/(2.0*hh);
		fac=CON2;
		for (j=1;j<=i;j++) {
			a[j][i]=(a[j-1][i]*fac-a[j-1][i-1])/(fac-1.0);
			fac=CON2*fac;
			errt=MAX(fabs(a[j][i]-a[j-1][i]),fabs(a[j][i]-a[j-1][i-1]));
			if (errt <= err) {
				err=errt;
				ans=a[j][i];
			}
		}
		if (fabs(a[i][i]-a[i-1][i-1]) >= SAFE*err) break;
	}
	return ans;
}
