#include "nr.h"

void NR::mp2dfr(Vec_IO_UCHR &a, string &s)
{
	const unsigned int IAZ=48;
	int j,m;

	int n=a.size();
	m=int(2.408*n);
	mplsh(a);
	for (j=0;j<m;j++) {
		mpsmu(a,a,10);
		s += a[0]+IAZ;
		mplsh(a);
	}
}
