#include <cmath>
#include "nr.h"
using namespace std;

void NR::frprmn(Vec_IO_DP &p, const DP ftol, int &iter, DP &fret,
	DP func(Vec_I_DP &), void dfunc(Vec_I_DP &, Vec_O_DP &))
{
	const int ITMAX=200;
	const DP EPS=1.0e-18;
	int j,its;
	DP gg,gam,fp,dgg;

	int n=p.size();
	Vec_DP g(n),h(n),xi(n);
	fp=func(p);
	dfunc(p,xi);
	for (j=0;j<n;j++) {
		g[j] = -xi[j];
		xi[j]=h[j]=g[j];
	}
	for (its=0;its<ITMAX;its++) {
		iter=its;
		linmin(p,xi,fret,func);
		if (2.0*fabs(fret-fp) <= ftol*(fabs(fret)+fabs(fp)+EPS))
			return;
		fp=fret;
		dfunc(p,xi);
		dgg=gg=0.0;
		for (j=0;j<n;j++) {
			gg += g[j]*g[j];
//		  dgg += xi[j]*xi[j];
			dgg += (xi[j]+g[j])*xi[j];
		}
		if (gg == 0.0)
			return;
		gam=dgg/gg;
		for (j=0;j<n;j++) {
			g[j] = -xi[j];
			xi[j]=h[j]=g[j]+gam*h[j];
		}
	}
	nrerror("Too many iterations in frprmn");
}
