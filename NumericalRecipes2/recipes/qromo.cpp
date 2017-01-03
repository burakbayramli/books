#include <cmath>
#include "nr.h"
using namespace std;

DP NR::qromo(DP func(const DP), const DP a, const DP b,
	DP choose(DP (*)(const DP), const DP, const DP, const int))
{
	const int JMAX=14, JMAXP=JMAX+1, K=5;
	const DP EPS=3.0e-9;
	int i,j;
	DP ss,dss;
	Vec_DP h(JMAXP),s(JMAX),h_t(K),s_t(K);

	h[0]=1.0;
	for (j=1;j<=JMAX;j++) {
		s[j-1]=choose(func,a,b,j);
		if (j >= K) {
			for (i=0;i<K;i++) {
				h_t[i]=h[j-K+i];
				s_t[i]=s[j-K+i];
			}
			polint(h_t,s_t,0.0,ss,dss);
			if (fabs(dss) <= EPS*fabs(ss)) return ss;
		}
		h[j]=h[j-1]/9.0;
	}
	nrerror("Too many steps in routine qromo");
	return 0.0;
}
