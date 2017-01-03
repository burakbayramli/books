#include "nr.h"

void NR::mrqcof(Vec_I_DP &x, Vec_I_DP &y, Vec_I_DP &sig, Vec_I_DP &a,
	Vec_I_BOOL &ia, Mat_O_DP &alpha, Vec_O_DP &beta, DP &chisq,
	void funcs(const DP, Vec_I_DP &,DP &, Vec_O_DP &))
{
	int i,j,k,l,m,mfit=0;
	DP ymod,wt,sig2i,dy;

	int ndata=x.size();
	int ma=a.size();
	Vec_DP dyda(ma);
	for (j=0;j<ma;j++)
		if (ia[j]) mfit++;
	for (j=0;j<mfit;j++) {
		for (k=0;k<=j;k++) alpha[j][k]=0.0;
		beta[j]=0.0;
	}
	chisq=0.0;
	for (i=0;i<ndata;i++) {
		funcs(x[i],a,ymod,dyda);
		sig2i=1.0/(sig[i]*sig[i]);
		dy=y[i]-ymod;
		for (j=0,l=0;l<ma;l++) {
			if (ia[l]) {
				wt=dyda[l]*sig2i;
				for (k=0,m=0;m<l+1;m++)
					if (ia[m]) alpha[j][k++] += wt*dyda[m];
				beta[j++] += dy*wt;
			}
		}
		chisq += dy*dy*sig2i;
	}
	for (j=1;j<mfit;j++)
		for (k=0;k<j;k++) alpha[k][j]=alpha[j][k];
}
