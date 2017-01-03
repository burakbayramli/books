#include <cmath>
#include "nr.h"
using namespace std;

void NR::airy(const DP x, DP &ai, DP &bi, DP &aip, DP &bip)
{
	const DP PI=3.141592653589793238, ONOVRT=0.577350269189626;
	const DP THIRD=(1.0/3.0), TWOTHR=2.0*THIRD;
	DP absx,ri,rip,rj,rjp,rk,rkp,rootx,ry,ryp,z;

	absx=fabs(x);
	rootx=sqrt(absx);
	z=TWOTHR*absx*rootx;
	if (x > 0.0) {
		bessik(z,THIRD,ri,rk,rip,rkp);
		ai=rootx*ONOVRT*rk/PI;
		bi=rootx*(rk/PI+2.0*ONOVRT*ri);
		bessik(z,TWOTHR,ri,rk,rip,rkp);
		aip = -x*ONOVRT*rk/PI;
		bip=x*(rk/PI+2.0*ONOVRT*ri);
	} else if (x < 0.0) {
		bessjy(z,THIRD,rj,ry,rjp,ryp);
		ai=0.5*rootx*(rj-ONOVRT*ry);
		bi = -0.5*rootx*(ry+ONOVRT*rj);
		bessjy(z,TWOTHR,rj,ry,rjp,ryp);
		aip=0.5*absx*(ONOVRT*ry+rj);
		bip=0.5*absx*(ONOVRT*rj-ry);
	} else {
		ai=0.355028053887817;
		bi=ai/ONOVRT;
		aip = -0.258819403792807;
		bip = -aip/ONOVRT;
	}
}
