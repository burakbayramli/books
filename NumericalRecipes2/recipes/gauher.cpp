#include <cmath>
#include "nr.h"
using namespace std;

void NR::gauher(Vec_O_DP &x, Vec_O_DP &w)
{
	const DP EPS=1.0e-14,PIM4=0.7511255444649425;
	const int MAXIT=10;
	int i,its,j,m;
	DP p1,p2,p3,pp,z,z1;

	int n=x.size();
	m=(n+1)/2;
	for (i=0;i<m;i++) {
		if (i == 0) {
			z=sqrt(DP(2*n+1))-1.85575*pow(DP(2*n+1),-0.16667);
		} else if (i == 1) {
			z -= 1.14*pow(DP(n),0.426)/z;
		} else if (i == 2) {
			z=1.86*z-0.86*x[0];
		} else if (i == 3) {
			z=1.91*z-0.91*x[1];
		} else {
			z=2.0*z-x[i-2];
		}
		for (its=0;its<MAXIT;its++) {
			p1=PIM4;
			p2=0.0;
			for (j=0;j<n;j++) {
				p3=p2;
				p2=p1;
				p1=z*sqrt(2.0/(j+1))*p2-sqrt(DP(j)/(j+1))*p3;
			}
			pp=sqrt(DP(2*n))*p2;
			z1=z;
			z=z1-p1/pp;
			if (fabs(z-z1) <= EPS) break;
		}
		if (its >= MAXIT) nrerror("too many iterations in gauher");
		x[i]=z;
		x[n-1-i] = -z;
		w[i]=2.0/(pp*pp);
		w[n-1-i]=w[i];
	}
}
