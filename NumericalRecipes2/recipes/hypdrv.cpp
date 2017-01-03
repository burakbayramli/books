#include <complex>
#include "nr.h"
using namespace std;

extern complex<DP> aa,bb,cc,z0,dz;

void NR::hypdrv(const DP s, Vec_I_DP &yy, Vec_O_DP &dyyds)
{
	complex<DP> z,y[2],dyds[2];

	y[0]=complex<DP>(yy[0],yy[1]);
	y[1]=complex<DP>(yy[2],yy[3]);
	z=z0+s*dz;
	dyds[0]=y[1]*dz;
	dyds[1]=(aa*bb*y[0]-(cc-(aa+bb+1.0)*z)*y[1])*dz/(z*(1.0-z));
	dyyds[0]=real(dyds[0]);
	dyyds[1]=imag(dyds[0]);
	dyyds[2]=real(dyds[1]);
	dyyds[3]=imag(dyds[1]);
}
