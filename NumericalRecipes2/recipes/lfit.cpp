#include "nr.h"

void NR::lfit(Vec_I_DP &x, Vec_I_DP &y, Vec_I_DP &sig, Vec_IO_DP &a,
	Vec_I_BOOL &ia, Mat_O_DP &covar, DP &chisq,
	void funcs(const DP, Vec_O_DP &))
{
	int i,j,k,l,m,mfit=0;
	DP ym,wt,sum,sig2i;

	int ndat=x.size();
	int ma=a.size();
	Vec_DP afunc(ma);
	Mat_DP beta(ma,1);
	for (j=0;j<ma;j++)
		if (ia[j]) mfit++;
	if (mfit == 0) nrerror("lfit: no parameters to be fitted");
	for (j=0;j<mfit;j++) {
		for (k=0;k<mfit;k++) covar[j][k]=0.0;
		beta[j][0]=0.0;
	}
	for (i=0;i<ndat;i++) {
		funcs(x[i],afunc);
		ym=y[i];
		if (mfit < ma) {
			for (j=0;j<ma;j++)
				if (!ia[j]) ym -= a[j]*afunc[j];
		}
		sig2i=1.0/SQR(sig[i]);
		for (j=0,l=0;l<ma;l++) {
			if (ia[l]) {
				wt=afunc[l]*sig2i;
				for (k=0,m=0;m<=l;m++)
					if (ia[m]) covar[j][k++] += wt*afunc[m];
				beta[j++][0] += ym*wt;
			}
		}
	}
	for (j=1;j<mfit;j++)
		for (k=0;k<j;k++)
			covar[k][j]=covar[j][k];
	Mat_DP temp(mfit,mfit);
	for (j=0;j<mfit;j++)
		for (k=0;k<mfit;k++)
			temp[j][k]=covar[j][k];
	gaussj(temp,beta);
	for (j=0;j<mfit;j++)
		for (k=0;k<mfit;k++)
			covar[j][k]=temp[j][k];
	for (j=0,l=0;l<ma;l++)
		if (ia[l]) a[l]=beta[j++][0];
	chisq=0.0;
	for (i=0;i<ndat;i++) {
		funcs(x[i],afunc);
		sum=0.0;
		for (j=0;j<ma;j++) sum += a[j]*afunc[j];
		chisq += SQR((y[i]-sum)/sig[i]);
	}
	covsrt(covar,ia,mfit);
}
