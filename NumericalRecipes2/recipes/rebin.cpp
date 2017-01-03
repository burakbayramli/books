#include "nr.h"

void NR::rebin(const DP rc, const int nd, Vec_I_DP &r, Vec_O_DP &xin,
	Mat_IO_DP &xi, const int j)
{
	int i,k=0;
	DP dr=0.0,xn=0.0,xo=0.0;

	for (i=0;i<nd-1;i++) {
		while (rc > dr)
			dr += r[(++k)-1];
		if (k > 1) xo=xi[j][k-2];
		xn=xi[j][k-1];
		dr -= rc;
		xin[i]=xn-(xn-xo)*dr/r[k-1];
	}
	for (i=0;i<nd-1;i++) xi[j][i]=xin[i];
	xi[j][nd-1]=1.0;
}
