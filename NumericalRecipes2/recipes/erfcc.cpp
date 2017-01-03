#include <cmath>
#include "nr.h"
using namespace std;

DP NR::erfcc(const DP x)
{
	DP t,z,ans;

	z=fabs(x);
	t=1.0/(1.0+0.5*z);
	ans=t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+
		t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+
		t*(-0.82215223+t*0.17087277)))))))));
	return (x >= 0.0 ? ans : 2.0-ans);
}
