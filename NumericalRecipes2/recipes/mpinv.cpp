#include "nr.h"

void NR::mpinv(Vec_O_UCHR &u, Vec_I_UCHR &v)
{
	const int MF=4;
	const DP BI=1.0/256.0;
	int i,j,mm;
	DP fu,fv;

	int n=u.size();
	int m=v.size();
	Vec_UCHR s(n+m),r(2*n+m);
	mm=MIN(MF,m);
	fv=DP(v[mm-1]);
	for (j=mm-2;j>=0;j--) {
		fv *= BI;
		fv += v[j];
	}
	fu=1.0/fv;
	for (j=0;j<n;j++) {
		i=int(fu);
		u[j]=(unsigned char) i;
		fu=256.0*(fu-i);
	}
	for (;;) {
		mpmul(s,u,v);
		mplsh(s);
		mpneg(s);
		s[0] += (unsigned char) 2;
		mpmul(r,s,u);
		mplsh(r);
		mpmov(u,r);
		for (j=1;j<n-1;j++)
			if (s[j] != 0) break;
		if (j==n-1) return;
	}
}
