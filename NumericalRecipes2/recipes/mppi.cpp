#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

void NR::mppi(const int np)
{
	const unsigned int IAOFF=48,MACC=2;
	int ir,j,n;
	unsigned char mm;
	string s;

	n=np+MACC;
	Vec_UCHR x(n),y(n),sx(n),sxi(n);
	Vec_UCHR z(n),t(n),pi(n);
	Vec_UCHR ss(2*n),tt(2*n);
	t[0]=2;
	for (j=1;j<n;j++) t[j]=0;
	mpsqrt(x,x,t);
	mpadd(pi,t,x);
	mplsh(pi);
	mpsqrt(sx,sxi,x);
	mpmov(y,sx);
	for (;;) {
		mpadd(z,sx,sxi);
		mplsh(z);
		mpsdv(x,z,2,ir);
		mpsqrt(sx,sxi,x);
		mpmul(tt,y,sx);
		mplsh(tt);
		mpadd(tt,tt,sxi);
		mplsh(tt);
		x[0]++;
		y[0]++;
		mpinv(ss,y);
		mpmul(y,tt,ss);
		mplsh(y);
		mpmul(tt,x,ss);
		mplsh(tt);
		mpmul(ss,pi,tt);
		mplsh(ss);
		mpmov(pi,ss);
		mm=tt[0]-1;
		for (j=1;j < n-1;j++)
			if (tt[j] != mm) break;
		if (j == n-1) {
			cout << endl << "pi        = ";
			s=pi[0]+IAOFF;
			s += '.';
			mp2dfr(pi,s);
			s.erase(2.408*np,s.length());
			cout << setw(64) << left << s << endl;
			return;
		}
	}
}
