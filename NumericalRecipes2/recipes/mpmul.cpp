#include "nr.h"

void NR::mpmul(Vec_O_UCHR &w, Vec_I_UCHR &u, Vec_I_UCHR &v)
{
	const DP RX=256.0;
	int j,n_max,nn=1;
	DP cy,t;

	int n=u.size();
	int m=v.size();
	int p=w.size();
	n_max=MAX(m,n);
	while (nn < n_max) nn <<= 1;
	nn <<= 1;
	Vec_DP a(0.0,nn),b(0.0,nn);
	for (j=0;j<n;j++) a[j]=u[j];
	for (j=0;j<m;j++) b[j]=v[j];
	realft(a,1);
	realft(b,1);
	b[0] *= a[0];
	b[1] *= a[1];
	for (j=2;j<nn;j+=2) {
		b[j]=(t=b[j])*a[j]-b[j+1]*a[j+1];
		b[j+1]=t*a[j+1]+b[j+1]*a[j];
	}
	realft(b,-1);
	cy=0.0;
	for (j=nn-1;j>=0;j--) {
		t=b[j]/(nn >> 1)+cy+0.5;
		cy=(unsigned long) (t/RX);
		b[j]=t-cy*RX;
	}
	if (cy >= RX) nrerror("cannot happen in mpmul");
	for (j=0;j<p;j++) w[j]=0;
	w[0]=(unsigned char) cy;
	for (j=1;j<MIN(n+m,p);j++)
		w[j]=(unsigned char) b[j-1];
}
