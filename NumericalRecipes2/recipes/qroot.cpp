#include <cmath>
#include "nr.h"
using namespace std;

void NR::qroot(Vec_I_DP &p, DP &b, DP &c, const DP eps)
{
	const int ITMAX=20;
	const DP TINY=1.0e-14;
	int iter;
	DP sc,sb,s,rc,rb,r,dv,delc,delb;
	Vec_DP d(3);

	int n=p.size()-1;
	Vec_DP q(n+1),qq(n+1),rem(n+1);
	d[2]=1.0;
	for (iter=0;iter<ITMAX;iter++) {
		d[1]=b;
		d[0]=c;
		poldiv(p,d,q,rem);
		s=rem[0];
		r=rem[1];
		poldiv(q,d,qq,rem);
		sb = -c*(rc = -rem[1]);
		rb = -b*rc+(sc = -rem[0]);
		dv=1.0/(sb*rc-sc*rb);
		delb=(r*sc-s*rc)*dv;
		delc=(-r*sb+s*rb)*dv;
		b += (delb=(r*sc-s*rc)*dv);
		c += (delc=(-r*sb+s*rb)*dv);
		if ((fabs(delb) <= eps*fabs(b) || fabs(b) < TINY)
			&& (fabs(delc) <= eps*fabs(c) || fabs(c) < TINY)) {
			return;
		}
	}
	nrerror("Too many iterations in routine qroot");
}
