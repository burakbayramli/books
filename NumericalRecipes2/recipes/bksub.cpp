#include "nr.h"

void NR::bksub(const int ne, const int nb, const int jf, const int k1,
	const int k2, Mat3D_IO_DP &c)
{
	int nbf,im,kp,k,j,i;
	DP xx;

	nbf=ne-nb;
	im=1;
	for (k=k2-1;k>=k1;k--) {
		if (k == k1) im=nbf+1;
		kp=k+1;
		for (j=0;j<nbf;j++) {
			xx=c[j][jf][kp];
			for (i=im-1;i<ne;i++)
				c[i][jf][k] -= c[i][j][k]*xx;
		}
	}
	for (k=k1;k<k2;k++) {
		kp=k+1;
		for (i=0;i<nb;i++) c[i][0][k]=c[i+nbf][jf][k];
		for (i=0;i<nbf;i++) c[i+nb][0][k]=c[i][jf][kp];
	}
}
