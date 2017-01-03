#include "nr.h"

void NR::covsrt(Mat_IO_DP &covar, Vec_I_BOOL &ia, const int mfit)
{
	int i,j,k;

	int ma=ia.size();
	for (i=mfit;i<ma;i++)
		for (j=0;j<i+1;j++) covar[i][j]=covar[j][i]=0.0;
	k=mfit-1;
	for (j=ma-1;j>=0;j--) {
		if (ia[j]) {
			for (i=0;i<ma;i++) SWAP(covar[i][k],covar[i][j]);
			for (i=0;i<ma;i++) SWAP(covar[k][i],covar[j][i]);
			k--;
		}
	}
}
