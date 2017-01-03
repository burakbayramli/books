#include "nr.h"

void NR::mpdiv(Vec_O_UCHR &q, Vec_O_UCHR &r, Vec_I_UCHR &u, Vec_I_UCHR &v)
{
	const int MACC=1;
	int i,is,mm;

	int n=u.size();
	int m=v.size();
	int p=r.size();
	int n_min=MIN(m,p);
	if (m > n) nrerror("Divisor longer than dividend in mpdiv");
	mm=m+MACC;
	Vec_UCHR s(mm),rr(mm),ss(mm+1),qq(n-m+1),t(n);
	mpinv(s,v);
	mpmul(rr,s,u);
	mpsad(ss,rr,1);
	mplsh(ss);
	mplsh(ss);
	mpmov(qq,ss);
	mpmov(q,qq);
	mpmul(t,qq,v);
	mplsh(t);
	mpsub(is,t,u,t);
	if (is != 0) nrerror("MACC too small in mpdiv");
	for (i=0;i<n_min;i++) r[i]=t[i+n-m];
	if (p>m)
		for (i=m;i<p;i++) r[i]=0;
}
