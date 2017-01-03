#include <cmath>
#include "nr.h"
using namespace std;

DP NR::evlmem(const DP fdt, Vec_I_DP &d, const DP xms)
{
	int i;
	DP sumr=1.0,sumi=0.0,wr=1.0,wi=0.0,wpr,wpi,wtemp,theta;

	int m=d.size();
	theta=6.28318530717959*fdt;
	wpr=cos(theta);
	wpi=sin(theta);
	for (i=0;i<m;i++) {
		wr=(wtemp=wr)*wpr-wi*wpi;
		wi=wi*wpr+wtemp*wpi;
		sumr -= d[i]*wr;
		sumi -= d[i]*wi;
	}
	return xms/(sumr*sumr+sumi*sumi);
}
