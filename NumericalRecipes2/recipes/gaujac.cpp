#include <cmath>
#include "nr.h"
using namespace std;

void NR::gaujac(Vec_O_DP &x, Vec_O_DP &w, const DP alf, const DP bet)
{
	const int MAXIT=10;
	const DP EPS=1.0e-14;
	int i,its,j;
	DP alfbet,an,bn,r1,r2,r3;
	DP a,b,c,p1,p2,p3,pp,temp,z,z1;

	int n=x.size();
	for (i=0;i<n;i++) {
		if (i == 0) {
			an=alf/n;
			bn=bet/n;
			r1=(1.0+alf)*(2.78/(4.0+n*n)+0.768*an/n);
			r2=1.0+1.48*an+0.96*bn+0.452*an*an+0.83*an*bn;
			z=1.0-r1/r2;
		} else if (i == 1) {
			r1=(4.1+alf)/((1.0+alf)*(1.0+0.156*alf));
			r2=1.0+0.06*(n-8.0)*(1.0+0.12*alf)/n;
			r3=1.0+0.012*bet*(1.0+0.25*fabs(alf))/n;
			z -= (1.0-z)*r1*r2*r3;
		} else if (i == 2) {
			r1=(1.67+0.28*alf)/(1.0+0.37*alf);
			r2=1.0+0.22*(n-8.0)/n;
			r3=1.0+8.0*bet/((6.28+bet)*n*n);
			z -= (x[0]-z)*r1*r2*r3;
		} else if (i == n-2) {
			r1=(1.0+0.235*bet)/(0.766+0.119*bet);
			r2=1.0/(1.0+0.639*(n-4.0)/(1.0+0.71*(n-4.0)));
			r3=1.0/(1.0+20.0*alf/((7.5+alf)*n*n));
			z += (z-x[n-4])*r1*r2*r3;
		} else if (i == n-1) {
			r1=(1.0+0.37*bet)/(1.67+0.28*bet);
			r2=1.0/(1.0+0.22*(n-8.0)/n);
			r3=1.0/(1.0+8.0*alf/((6.28+alf)*n*n));
			z += (z-x[n-3])*r1*r2*r3;
		} else {
			z=3.0*x[i-1]-3.0*x[i-2]+x[i-3];
		}
		alfbet=alf+bet;
		for (its=1;its<=MAXIT;its++) {
			temp=2.0+alfbet;
			p1=(alf-bet+temp*z)/2.0;
			p2=1.0;
			for (j=2;j<=n;j++) {
				p3=p2;
				p2=p1;
				temp=2*j+alfbet;
				a=2*j*(j+alfbet)*(temp-2.0);
				b=(temp-1.0)*(alf*alf-bet*bet+temp*(temp-2.0)*z);
				c=2.0*(j-1+alf)*(j-1+bet)*temp;
				p1=(b*p2-c*p3)/a;
			}
			pp=(n*(alf-bet-temp*z)*p1+2.0*(n+alf)*(n+bet)*p2)/(temp*(1.0-z*z));
			z1=z;
			z=z1-p1/pp;
			if (fabs(z-z1) <= EPS) break;
		}
		if (its > MAXIT) nrerror("too many iterations in gaujac");
		x[i]=z;
		w[i]=exp(gammln(alf+n)+gammln(bet+n)-gammln(n+1.0)-
			gammln(n+alfbet+1.0))*temp*pow(2.0,alfbet)/(pp*p2);
	}
}
