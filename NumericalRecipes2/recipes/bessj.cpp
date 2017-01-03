#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

DP NR::bessj(const int n, const DP x)
{
	const DP ACC=160.0;
	const int IEXP=numeric_limits<DP>::max_exponent/2;
	bool jsum;
	int j,k,m;
	DP ax,bj,bjm,bjp,dum,sum,tox,ans;

	if (n < 2) nrerror("Index n less than 2 in bessj");
	ax=fabs(x);
	if (ax*ax <= 8.0*numeric_limits<DP>::min()) return 0.0;
	else if (ax > DP(n)) {
		tox=2.0/ax;
		bjm=bessj0(ax);
		bj=bessj1(ax);
		for (j=1;j<n;j++) {
			bjp=j*tox*bj-bjm;
			bjm=bj;
			bj=bjp;
		}
		ans=bj;
	} else {
		tox=2.0/ax;
		m=2*((n+int(sqrt(ACC*n)))/2);
		jsum=false;
		bjp=ans=sum=0.0;
		bj=1.0;
		for (j=m;j>0;j--) {
			bjm=j*tox*bj-bjp;
			bjp=bj;
			bj=bjm;
			dum=frexp(bj,&k);
			if (k > IEXP) {
				bj=ldexp(bj,-IEXP);
				bjp=ldexp(bjp,-IEXP);
				ans=ldexp(ans,-IEXP);
				sum=ldexp(sum,-IEXP);
			}
			if (jsum) sum += bj;
			jsum=!jsum;
			if (j == n) ans=bjp;
		}
		sum=2.0*sum-bj;
		ans /= sum;
	}
	return x < 0.0 && (n & 1) ? -ans : ans;
}
