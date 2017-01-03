#include "nr.h"

void NR::wt1(Vec_IO_DP &a, const int isign,
	void wtstep(Vec_IO_DP &, const int, const int))
{
	int nn;

	int n=a.size();
	if (n < 4) return;
	if (isign >= 0) {
		for (nn=n;nn>=4;nn>>=1) wtstep(a,nn,isign);
	} else {
		for (nn=4;nn<=n;nn<<=1) wtstep(a,nn,isign);
	}
}
