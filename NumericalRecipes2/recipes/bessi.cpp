#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

DP NR::bessi(const int n, const DP x)
{
	const DP ACC=200.0;
	const int IEXP=numeric_limits<DP>::max_exponent/2;
	int j,k;
	DP bi,bim,bip,dum,tox,ans;

	if (n < 2) nrerror("Index n less than 2 in bessi");
	if (x*x <= 8.0*numeric_limits<DP>::min()) return 0.0;
	else {
		tox=2.0/fabs(x);
		bip=ans=0.0;
		bi=1.0;
		for (j=2*(n+int(sqrt(ACC*n)));j>0;j--) {
			bim=bip+j*tox*bi;
			bip=bi;
			bi=bim;
			dum=frexp(bi,&k);
			if (k > IEXP) {
				ans=ldexp(ans,-IEXP);
				bi=ldexp(bi,-IEXP);
				bip=ldexp(bip,-IEXP);
			}
			if (j == n) ans=bip;
		}
		ans *= bessi0(x)/bi;
		return x < 0.0 && (n & 1) ? -ans : ans;
	}
}
