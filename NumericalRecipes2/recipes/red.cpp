#include "nr.h"

void NR::red(const int iz1, const int iz2, const int jz1, const int jz2,
	const int jm1, const int jm2, const int jmf, const int ic1,
	const int jc1, const int jcf, const int kc, Mat3D_I_DP &c,
	Mat_IO_DP &s)
{
	int loff,l,j,ic,i;
	DP vx;

	loff=jc1-jm1;
	ic=ic1;
	for (j=jz1;j<jz2;j++) {
		for (l=jm1;l<jm2;l++) {
			vx=c[ic][l+loff][kc-1];
			for (i=iz1;i<iz2;i++) s[i][l] -= s[i][j]*vx;
		}
		vx=c[ic][jcf][kc-1];
		for (i=iz1;i<iz2;i++) s[i][jmf] -= s[i][j]*vx;
		ic += 1;
	}
}
