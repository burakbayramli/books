#include "nr.h"

void NR::arcsum(Vec_I_ULNG &iin, Vec_O_ULNG &iout, unsigned long ja,
	const int nwk, const unsigned long nrad, const unsigned long nc)
{
	int karry=0;
	unsigned long j,jtmp;

	for (j=nwk-1;j>nc;j--) {
		jtmp=ja;
		ja /= nrad;
		iout[j]=iin[j]+(jtmp-ja*nrad)+karry;
		if (iout[j] >= nrad) {
			iout[j] -= nrad;
			karry=1;
		} else karry=0;
	}
	iout[nc]=iin[nc]+ja+karry;
}
