#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

extern DP aa,abdevt;
extern const Vec_DP *xt_p,*yt_p;

DP NR::rofunc(const DP b)
{
	const DP EPS=numeric_limits<DP>::epsilon();
	int j;
	DP d,sum=0.0;

	const Vec_DP &xt=*xt_p,&yt=*yt_p;
	int ndatat=xt.size();
	Vec_DP arr(ndatat);
	for (j=0;j<ndatat;j++) arr[j]=yt[j]-b*xt[j];
	if (ndatat & 1 == 1) {
		aa=select((ndatat-1)>>1,arr);
	} else {
		j=ndatat >> 1;
		aa=0.5*(select(j-1,arr)+select(j,arr));
	}
	abdevt=0.0;
	for (j=0;j<ndatat;j++) {
		d=yt[j]-(b*xt[j]+aa);
		abdevt += fabs(d);
		if (yt[j] != 0.0) d /= fabs(yt[j]);
		if (fabs(d) > EPS) sum += (d >= 0.0 ? xt[j] : -xt[j]);
	}
	return sum;
}
