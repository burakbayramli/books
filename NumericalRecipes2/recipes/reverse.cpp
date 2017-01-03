#include "nr.h"

void NR::reverse(Vec_IO_INT &iorder, Vec_I_INT &n)
{
	int nn,j,k,l,itmp;

	int ncity=iorder.size();
	nn=(1+((n[1]-n[0]+ncity) % ncity))/2;
	for (j=0;j<nn;j++) {
		k=(n[0]+j) % ncity;
		l=(n[1]-j+ncity) % ncity;
		itmp=iorder[k];
		iorder[k]=iorder[l];
		iorder[l]=itmp;
	}
}
