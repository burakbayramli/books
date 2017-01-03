#include <cmath>
#include "nr.h"
using namespace std;

void NR::sncndn(const DP uu, const DP emmc, DP &sn, DP &cn, DP &dn)
{
	const DP CA=1.0e-8;
	bool bo;
	int i,ii,l;
	DP a,b,c,d,emc,u;
	Vec_DP em(13),en(13);

	emc=emmc;
	u=uu;
	if (emc != 0.0) {
		bo=(emc < 0.0);
		if (bo) {
			d=1.0-emc;
			emc /= -1.0/d;
			u *= (d=sqrt(d));
		}
		a=1.0;
		dn=1.0;
		for (i=0;i<13;i++) {
			l=i;
			em[i]=a;
			en[i]=(emc=sqrt(emc));
			c=0.5*(a+emc);
			if (fabs(a-emc) <= CA*a) break;
			emc *= a;
			a=c;
		}
		u *= c;
		sn=sin(u);
		cn=cos(u);
		if (sn != 0.0) {
			a=cn/sn;
			c *= a;
			for (ii=l;ii>=0;ii--) {
				b=em[ii];
				a *= c;
				c *= dn;
				dn=(en[ii]+a)/(b+a);
				a=c/b;
			}
			a=1.0/sqrt(c*c+1.0);
			sn=(sn >= 0.0 ? a : -a);
			cn=c*sn;
		}
		if (bo) {
			a=dn;
			dn=cn;
			cn=a;
			sn /= d;
		}
	} else {
		cn=1.0/cosh(u);
		dn=cn;
		sn=tanh(u);
	}
}
