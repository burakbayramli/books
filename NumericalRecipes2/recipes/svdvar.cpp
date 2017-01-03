#include "nr.h"

void NR::svdvar(Mat_I_DP &v, Vec_I_DP &w, Mat_O_DP &cvm)
{
	int i,j,k;
	DP sum;

	int ma=w.size();
	Vec_DP wti(ma);
	for (i=0;i<ma;i++) {
		wti[i]=0.0;
		if (w[i] != 0.0) wti[i]=1.0/(w[i]*w[i]);
	}
	for (i=0;i<ma;i++) {
		for (j=0;j<i+1;j++) {
			sum=0.0;
			for (k=0;k<ma;k++)
				sum += v[i][k]*v[j][k]*wti[k];
			cvm[j][i]=cvm[i][j]=sum;
		}
	}
}
