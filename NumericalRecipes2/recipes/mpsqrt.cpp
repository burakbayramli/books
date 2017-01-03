#include <cmath>
#include "nr.h"
using namespace std;

void NR::mpsqrt(Vec_O_UCHR &w, Vec_O_UCHR &u, Vec_I_UCHR &v)
{
	const int MF=3;
	const DP BI=1.0/256.0;
	int i,ir,j,mm;
	DP fu,fv;

	int n=u.size();
	int m=v.size();
	Vec_UCHR r(2*n),x(n+m),s(2*n+m),t(3*n+m);
	mm=MIN(m,MF);
	fv=DP(v[mm-1]);
	for (j=mm-2;j>=0;j--) {
		fv *= BI;
		fv += v[j];
	}
	fu=1.0/sqrt(fv);
	for (j=0;j<n;j++) {
		i=int(fu);
		u[j]=(unsigned char) i;
		fu=256.0*(fu-i);
	}
	for (;;) {
		mpmul(r,u,u);
		mplsh(r);
		mpmul(s,r,v);
		mplsh(s);
		mpneg(s);
		s[0] += (unsigned char) 3;
		mpsdv(s,s,2,ir);
		for (j=1;j<n-1;j++) {
			if (s[j] != 0) {
				mpmul(t,s,u);
				mplsh(t);
				mpmov(u,t);
				break;
			}
		}
		if (j<n-1) continue;
		mpmul(x,u,v);
		mplsh(x);
		mpmov(w,x);
		return;
	}
}
