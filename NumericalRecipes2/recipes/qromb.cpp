#include <cmath>
#include "nr.h"
using namespace std;

DP NR::qromb(DP func(const DP), DP a, DP b)
{
	const int JMAX=20, JMAXP=JMAX+1, K=5;
	const DP EPS=1.0e-10;
	DP ss,dss;
	Vec_DP s(JMAX),h(JMAXP),s_t(K),h_t(K);
	int i,j;

	h[0]=1.0;
	for (j=1;j<=JMAX;j++) {
		s[j-1]=trapzd(func,a,b,j);
		if (j >= K) {
			for (i=0;i<K;i++) {
				h_t[i]=h[j-K+i];
				s_t[i]=s[j-K+i];
			}
			polint(h_t,s_t,0.0,ss,dss);
			if (fabs(dss) <= EPS*fabs(ss)) return ss;
		}
		h[j]=0.25*h[j-1];
	}
	nrerror("Too many steps in routine qromb");
	return 0.0;
}
