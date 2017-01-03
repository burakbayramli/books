#include "nr.h"

void NR::addint(Mat_O_DP &uf, Mat_I_DP &uc, Mat_O_DP &res)
{
	int i,j;

	int nf=uf.nrows();
	interp(res,uc);
	for (j=0;j<nf;j++)
		for (i=0;i<nf;i++)
			uf[i][j] += res[i][j];
}
