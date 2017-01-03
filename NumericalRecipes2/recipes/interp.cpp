#include "nr.h"

void NR::interp(Mat_O_DP &uf, Mat_I_DP &uc)
{
	int ic,iif,jc,jf,nc;

	int nf=uf.nrows();
	nc=nf/2+1;
	for (jc=0;jc<nc;jc++)
		for (ic=0;ic<nc;ic++) uf[2*ic][2*jc]=uc[ic][jc];
	for (jf=0;jf<nf;jf+=2)
		for (iif=1;iif<nf-1;iif+=2)
			uf[iif][jf]=0.5*(uf[iif+1][jf]+uf[iif-1][jf]);
	for (jf=1;jf<nf-1;jf+=2)
		for (iif=0;iif<nf;iif++)
			uf[iif][jf]=0.5*(uf[iif][jf+1]+uf[iif][jf-1]);
}
