#include <cmath>
#include <complex>
#include "nr.h"
using namespace std;

complex<DP> aa,bb,cc,z0,dz;

int kmax,kount;
DP dxsav;
Vec_DP *xp_p;
Mat_DP *yp_p;

complex<DP> NR::hypgeo(const complex<DP> &a, const complex<DP> &b,
	const complex<DP> &c, const complex<DP> &z)
{
	const DP EPS=1.0e-14;
	int nbad,nok;
	complex<DP> ans,y[2];
	Vec_DP yy(4);

	kmax=0;
	if (norm(z) <= 0.25) {
		hypser(a,b,c,z,ans,y[1]);
		return ans;
	}
	else if (real(z) < 0.0) z0=complex<DP>(-0.5,0.0);
	else if (real(z) <= 1.0) z0=complex<DP>(0.5,0.0);
	else z0=complex<DP>(0.0,imag(z) >= 0.0 ? 0.5 : -0.5);
	aa=a;
	bb=b;
	cc=c;
	dz=z-z0;
	hypser(aa,bb,cc,z0,y[0],y[1]);
	yy[0]=real(y[0]);
	yy[1]=imag(y[0]);
	yy[2]=real(y[1]);
	yy[3]=imag(y[1]);
	odeint(yy,0.0,1.0,EPS,0.1,0.0001,nok,nbad,hypdrv,bsstep);
	y[0]=complex<DP>(yy[0],yy[1]);
	return y[0];
}
