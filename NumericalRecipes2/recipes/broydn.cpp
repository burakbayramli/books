#include <cmath>
#include <limits>
#include "nr.h"
using namespace std;

Vec_DP *fvec_p;
void (*nrfuncv)(Vec_I_DP &v, Vec_O_DP &f);

void NR::broydn(Vec_IO_DP &x, bool &check, void vecfunc(Vec_I_DP &, Vec_O_DP &))
{
	const int MAXITS=200;
	const DP EPS=numeric_limits<DP>::epsilon();
	const DP TOLF=1.0e-8, TOLX=EPS, STPMX=100.0, TOLMIN=1.0e-12;
	bool restrt,sing,skip;
	int i,its,j,k;
	DP den,f,fold,stpmax,sum,temp,test;

	int n=x.size();
	Mat_DP qt(n,n),r(n,n);
	Vec_DP c(n),d(n),fvcold(n),g(n),p(n),s(n),t(n),w(n),xold(n);
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
	for (sum=0.0,i=0;i<n;i++) sum += SQR(x[i]);
	stpmax=STPMX*MAX(sqrt(sum),DP(n));
	restrt=true;
	for (its=1;its<=MAXITS;its++) {
		if (restrt) {
			fdjac(x,fvec,r,vecfunc);
			qrdcmp(r,c,d,sing);
			if (sing) nrerror("singular Jacobian in broydn");
			for (i=0;i<n;i++) {
				for (j=0;j<n;j++) qt[i][j]=0.0;
				qt[i][i]=1.0;
			}
			for (k=0;k<n-1;k++) {
				if (c[k] != 0.0) {
					for (j=0;j<n;j++) {
						sum=0.0;
						for (i=k;i<n;i++)
							sum += r[i][k]*qt[i][j];
						sum /= c[k];
						for (i=k;i<n;i++)
							qt[i][j] -= sum*r[i][k];
					}
				}
			}
			for (i=0;i<n;i++) {
				r[i][i]=d[i];
				for (j=0;j<i;j++) r[i][j]=0.0;
			}
		} else {
			for (i=0;i<n;i++) s[i]=x[i]-xold[i];
			for (i=0;i<n;i++) {
				for (sum=0.0,j=i;j<n;j++) sum += r[i][j]*s[j];
				t[i]=sum;
			}
			skip=true;
			for (i=0;i<n;i++) {
				for (sum=0.0,j=0;j<n;j++) sum += qt[j][i]*t[j];
				w[i]=fvec[i]-fvcold[i]-sum;
				if (fabs(w[i]) >= EPS*(fabs(fvec[i])+fabs(fvcold[i]))) skip=false;
				else w[i]=0.0;
			}
			if (!skip) {
				for (i=0;i<n;i++) {
					for (sum=0.0,j=0;j<n;j++) sum += qt[i][j]*w[j];
					t[i]=sum;
				}
				for (den=0.0,i=0;i<n;i++) den += SQR(s[i]);
				for (i=0;i<n;i++) s[i] /= den;
				qrupdt(r,qt,t,s);
				for (i=0;i<n;i++) {
					if (r[i][i] == 0.0) nrerror("r singular in broydn");
					d[i]=r[i][i];
				}
			}
		}
		for (i=0;i<n;i++) {
			for (sum=0.0,j=0;j<n;j++) sum += qt[i][j]*fvec[j];
			p[i] = -sum;
		}
		for (i=n-1;i>=0;i--) {
			for (sum=0.0,j=0;j<=i;j++) sum -= r[j][i]*p[j];
			g[i]=sum;
		}
		for (i=0;i<n;i++) {
			xold[i]=x[i];
			fvcold[i]=fvec[i];
		}
		fold=f;
		rsolv(r,d,p);
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
			if (restrt) {
				delete fvec_p;
				return;
			} else {
				test=0.0;
				den=MAX(f,0.5*n);
				for (i=0;i<n;i++) {
					temp=fabs(g[i])*MAX(fabs(x[i]),1.0)/den;
					if (temp > test) test=temp;
				}
				if (test < TOLMIN) {
					delete fvec_p;
					return;
				}
				else restrt=true;
			}
		} else {
			restrt=false;
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
	}
	nrerror("MAXITS exceeded in broydn");
	return;
}
