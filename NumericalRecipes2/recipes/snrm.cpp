#include <cmath>
#include "nr.h"
using namespace std;

DP NR::snrm(Vec_I_DP &sx, const int itol)
{
	int i,isamax;
	DP ans;

	int n=sx.size();
	if (itol <= 3) {
		ans = 0.0;
		for (i=0;i<n;i++) ans += sx[i]*sx[i];
		return sqrt(ans);
	} else {
		isamax=0;
		for (i=0;i<n;i++) {
			if (fabs(sx[i]) > fabs(sx[isamax])) isamax=i;
		}
		return fabs(sx[isamax]);
	}
}
