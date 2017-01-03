#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

Vec_DP *fvec_p;
void (*nrfuncv)(Vec_I_DP &v, Vec_O_DP &f);

void NR::newt(Vec_IO_DP &x, bool &check, void vecfunc(Vec_I_DP &, Vec_O_DP &))
{
	const int MAXITS=200;
	const DP TOLF=1.0e-8,TOLMIN=1.0e-12,STPMX=100.0;
	const DP TOLX=numeric_limits<DP>::epsilon();
	int i,j,its;
	DP d,den,f,fold,stpmax,sum,temp,test;

	int n=x.size();
	Vec_INT indx(n);
	Vec_DP g(n),p(n),xold(n);
	Mat_DP fjac(n,n);
	fvec_p=new Vec_DP(n);
	nrfuncv=vecfunc;
	Vec_DP &fvec=*fvec_p;
	f=fmin(x);
	test=0.0;
	for (i=0;i<n;i++)
		if (fabs(fvec[i]) > test) test=fabs(fvec[i]);
	if (test < 0.01*TOLF) {
		check=false;
		delete fvec_p;
		return;
	}
	sum=0.0;
	for (i=0;i<n;i++) sum += SQR(x[i]);
	stpmax=STPMX*MAX(sqrt(sum),DP(n));
	for (its=0;its<MAXITS;its++) {
		fdjac(x,fvec,fjac,vecfunc);
		for (i=0;i<n;i++) {
			sum=0.0;
			for (j=0;j<n;j++) sum += fjac[j][i]*fvec[j];
			g[i]=sum;
		}
		for (i=0;i<n;i++) xold[i]=x[i];
		fold=f;
		for (i=0;i<n;i++) p[i] = -fvec[i];
		ludcmp(fjac,indx,d);
		lubksb(fjac,indx,p);
		lnsrch(xold,fold,g,p,x,f,stpmax,check,fmin);
		test=0.0;
		for (i=0;i<n;i++)
			if (fabs(fvec[i]) > test) test=fabs(fvec[i]);
		if (test < TOLF) {
			check=false;
			delete fvec_p;
			return;
		}
		if (check) {
			test=0.0;
			den=MAX(f,0.5*n);
			for (i=0;i<n;i++) {
				temp=fabs(g[i])*MAX(fabs(x[i]),1.0)/den;
				if (temp > test) test=temp;
			}
			check=(test < TOLMIN);
			delete fvec_p;
			return;
		}
		test=0.0;
		for (i=0;i<n;i++) {
			temp=(fabs(x[i]-xold[i]))/MAX(fabs(x[i]),1.0);
			if (temp > test) test=temp;
		}
		if (test < TOLX) {
			delete fvec_p;
			return;
		}
	}
	nrerror("MAXITS exceeded in newt");
}
