#include "nr.h"

void NR::svdfit(Vec_I_DP &x, Vec_I_DP &y, Vec_I_DP &sig, Vec_O_DP &a,
	Mat_O_DP &u, Mat_O_DP &v, Vec_O_DP &w, DP &chisq,
	void funcs(const DP, Vec_O_DP &))
{
	int i,j;
	const DP TOL=1.0e-13;
	DP wmax,tmp,thresh,sum;

	int ndata=x.size();
	int ma=a.size();
	Vec_DP b(ndata),afunc(ma);
	for (i=0;i<ndata;i++) {
		funcs(x[i],afunc);
		tmp=1.0/sig[i];
		for (j=0;j<ma;j++) u[i][j]=afunc[j]*tmp;
		b[i]=y[i]*tmp;
	}
	svdcmp(u,w,v);
	wmax=0.0;
	for (j=0;j<ma;j++)
		if (w[j] > wmax) wmax=w[j];
	thresh=TOL*wmax;
	for (j=0;j<ma;j++)
		if (w[j] < thresh) w[j]=0.0;
	svbksb(u,w,v,b,a);
	chisq=0.0;
	for (i=0;i<ndata;i++) {
		funcs(x[i],afunc);
		sum=0.0;
		for (j=0;j<ma;j++) sum += a[j]*afunc[j];
		chisq += (tmp=(y[i]-sum)/sig[i],tmp*tmp);
	}
}
