#include <cmath>
#include "nr.h"
using namespace std;

void NR::ksone(Vec_IO_DP &data, DP func(const DP), DP &d, DP &prob)
{
	int j;
	DP dt,en,ff,fn,fo=0.0;

	int n=data.size();
	sort(data);
	en=n;
	d=0.0;
	for (j=0;j<n;j++) {
		fn=(j+1)/en;
		ff=func(data[j]);
		dt=MAX(fabs(fo-ff),fabs(fn-ff));
		if (dt > d) d=dt;
		fo=fn;
	}
	en=sqrt(en);
	prob=probks((en+0.12+0.11/en)*d);
}
