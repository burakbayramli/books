#include <cmath>
#include "nr.h"
using namespace std;

void NR::moment(Vec_I_DP &data, DP &ave, DP &adev, DP &sdev, DP &var, DP &skew,
	DP &curt)
{
	int j;
	DP ep=0.0,s,p;

	int n=data.size();
	if (n <= 1) nrerror("n must be at least 2 in moment");
	s=0.0;
	for (j=0;j<n;j++) s += data[j];
	ave=s/n;
	adev=var=skew=curt=0.0;
	for (j=0;j<n;j++) {
		adev += fabs(s=data[j]-ave);
		ep += s;
		var += (p=s*s);
		skew += (p *= s);
		curt += (p *= s);
	}
	adev /= n;
	var=(var-ep*ep/n)/(n-1);
	sdev=sqrt(var);
	if (var != 0.0) {
		skew /= (n*var*sdev);
		curt=curt/(n*var*var)-3.0;
	} else nrerror("No skew/kurtosis when variance = 0 (in moment)");
}
