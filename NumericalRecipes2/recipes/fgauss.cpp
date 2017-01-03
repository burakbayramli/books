#include <cmath>
#include "nr.h"
using namespace std;

void NR::fgauss(const DP x, Vec_I_DP &a, DP &y, Vec_O_DP &dyda)
{
	int i;
	DP fac,ex,arg;

	int na=a.size();
	y=0.0;
	for (i=0;i<na-1;i+=3) {
		arg=(x-a[i+1])/a[i+2];
		ex=exp(-arg*arg);
		fac=a[i]*ex*2.0*arg;
		y += a[i]*ex;
		dyda[i]=ex;
		dyda[i+1]=fac/a[i+2];
		dyda[i+2]=fac*arg/a[i+2];
	}
}
