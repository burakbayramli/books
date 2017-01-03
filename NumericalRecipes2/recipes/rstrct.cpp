#include "nr.h"

void NR::rstrct(Mat_O_DP &uc, Mat_I_DP &uf)
{
	int ic,iif,jc,jf,ncc;

	int nc=uc.nrows();
	ncc=2*nc-2;
	for (jf=2,jc=1;jc<nc-1;jc++,jf+=2) {
		for (iif=2,ic=1;ic<nc-1;ic++,iif+=2) {
			uc[ic][jc]=0.5*uf[iif][jf]+0.125*(uf[iif+1][jf]+uf[iif-1][jf]
				+uf[iif][jf+1]+uf[iif][jf-1]);
		}
	}
	for (jc=0,ic=0;ic<nc;ic++,jc+=2) {
		uc[ic][0]=uf[jc][0];
		uc[ic][nc-1]=uf[jc][ncc];
	}
	for (jc=0,ic=0;ic<nc;ic++,jc+=2) {
		uc[0][ic]=uf[0][jc];
		uc[nc-1][ic]=uf[ncc][jc];
	}
}
