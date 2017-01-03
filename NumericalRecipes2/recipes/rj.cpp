#include <cmath>
#include "nr.h"
using namespace std;

DP NR::rj(const DP x, const DP y, const DP z, const DP p)
{
	const DP ERRTOL=0.0015, TINY=2.5e-13, BIG=9.0e11;
	const DP C1=3.0/14.0, C2=1.0/3.0, C3=3.0/22.0, C4=3.0/26.0;
	const DP C5=0.75*C3, C6=1.5*C4, C7=0.5*C2, C8=C3+C3;
	DP a,alamb,alpha,ans,ave,b,beta,delp,delx,dely,delz,ea,eb,ec,ed,ee,
		fac,pt,rcx,rho,sqrtx,sqrty,sqrtz,sum,tau,xt,yt,zt;

	if (MIN(MIN(x,y),z) < 0.0 || MIN(MIN(x+y,x+z),MIN(y+z,fabs(p))) < TINY
		|| MAX(MAX(x,y),MAX(z,fabs(p))) > BIG)
			nrerror("invalid arguments in rj");
	sum=0.0;
	fac=1.0;
	if (p > 0.0) {
		xt=x;
		yt=y;
		zt=z;
		pt=p;
	} else {
		xt=MIN(MIN(x,y),z);
		zt=MAX(MAX(x,y),z);
		yt=x+y+z-xt-zt;
		a=1.0/(yt-p);
		b=a*(zt-yt)*(yt-xt);
		pt=yt+b;
		rho=xt*zt/yt;
		tau=p*pt/yt;
		rcx=rc(rho,tau);
	}
	do {
		sqrtx=sqrt(xt);
		sqrty=sqrt(yt);
		sqrtz=sqrt(zt);
		alamb=sqrtx*(sqrty+sqrtz)+sqrty*sqrtz;
		alpha=SQR(pt*(sqrtx+sqrty+sqrtz)+sqrtx*sqrty*sqrtz);
		beta=pt*SQR(pt+alamb);
		sum += fac*rc(alpha,beta);
		fac=0.25*fac;
		xt=0.25*(xt+alamb);
		yt=0.25*(yt+alamb);
		zt=0.25*(zt+alamb);
		pt=0.25*(pt+alamb);
		ave=0.2*(xt+yt+zt+pt+pt);
		delx=(ave-xt)/ave;
		dely=(ave-yt)/ave;
		delz=(ave-zt)/ave;
		delp=(ave-pt)/ave;
	} while (MAX(MAX(fabs(delx),fabs(dely)),
		MAX(fabs(delz),fabs(delp))) > ERRTOL);
	ea=delx*(dely+delz)+dely*delz;
	eb=delx*dely*delz;
	ec=delp*delp;
	ed=ea-3.0*ec;
	ee=eb+2.0*delp*(ea-ec);
	ans=3.0*sum+fac*(1.0+ed*(-C1+C5*ed-C6*ee)+eb*(C7+delp*(-C8+delp*C4))
		+delp*ea*(C2-delp*C3)-C2*delp*ec)/(ave*sqrt(ave));
	if (p <= 0.0) ans=a*(b*ans+3.0*(rcx-rf(xt,yt,zt)));
	return ans;
}
