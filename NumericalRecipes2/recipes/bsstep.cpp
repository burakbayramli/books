#include <cmath>
#include "nr.h"
using namespace std;

Vec_DP *x_p;
Mat_DP *d_p;

void NR::bsstep(Vec_IO_DP &y, Vec_IO_DP &dydx, DP &xx, const DP htry,
	const DP eps, Vec_I_DP &yscal, DP &hdid, DP &hnext,
	void derivs(const DP, Vec_I_DP &, Vec_O_DP &))
{
	const int KMAXX=8, IMAXX=(KMAXX+1);
	const DP SAFE1=0.25, SAFE2=0.7, REDMAX=1.0e-5, REDMIN=0.7;
	const DP TINY=1.0e-30, SCALMX=0.1;
	static const int nseq_d[IMAXX]={2,4,6,8,10,12,14,16,18};
	static int first=1,kmax,kopt;
	static DP epsold = -1.0,xnew;
	static Vec_DP a(IMAXX);
	static Mat_DP alf(KMAXX,KMAXX);
	bool exitflag=false;
	int i,iq,k,kk,km,reduct;
	DP eps1,errmax,fact,h,red,scale,work,wrkmin,xest;
	Vec_INT nseq(nseq_d,IMAXX);
	Vec_DP err(KMAXX);

	int nv=y.size();
	Vec_DP yerr(nv),ysav(nv),yseq(nv);
	x_p=new Vec_DP(KMAXX);
	d_p=new Mat_DP(nv,KMAXX);
	if (eps != epsold) {
		hnext = xnew = -1.0e29;
		eps1=SAFE1*eps;
		a[0]=nseq[0]+1;
		for (k=0;k<KMAXX;k++) a[k+1]=a[k]+nseq[k+1];
		for (iq=1;iq<KMAXX;iq++) {
			for (k=0;k<iq;k++)
				alf[k][iq]=pow(eps1,(a[k+1]-a[iq+1])/
					((a[iq+1]-a[0]+1.0)*(2*k+3)));
		}
		epsold=eps;
		for (kopt=1;kopt<KMAXX-1;kopt++)
			if (a[kopt+1] > a[kopt]*alf[kopt-1][kopt]) break;
		kmax=kopt;
	}
	h=htry;
	for (i=0;i<nv;i++) ysav[i]=y[i];
	if (xx != xnew || h != hnext) {
		first=1;
		kopt=kmax;
	}
	reduct=0;
	for (;;) {
		for (k=0;k<=kmax;k++) {
			xnew=xx+h;
			if (xnew == xx) nrerror("step size underflow in bsstep");
			mmid(ysav,dydx,xx,h,nseq[k],yseq,derivs);
			xest=SQR(h/nseq[k]);
			pzextr(k,xest,yseq,y,yerr);
			if (k != 0) {
				errmax=TINY;
				for (i=0;i<nv;i++) errmax=MAX(errmax,fabs(yerr[i]/yscal[i]));
				errmax /= eps;
				km=k-1;
				err[km]=pow(errmax/SAFE1,1.0/(2*km+3));
			}
			if (k != 0 && (k >= kopt-1 || first)) {
				if (errmax < 1.0) {
					exitflag=true;
					break;
				}
				if (k == kmax || k == kopt+1) {
					red=SAFE2/err[km];
					break;
				}
				else if (k == kopt && alf[kopt-1][kopt] < err[km]) {
					red=1.0/err[km];
					break;
				}
				else if (kopt == kmax && alf[km][kmax-1] < err[km]) {
					red=alf[km][kmax-1]*SAFE2/err[km];
					break;
				}
				else if (alf[km][kopt] < err[km]) {
					red=alf[km][kopt-1]/err[km];
					break;
				}
			}
		}
		if (exitflag) break;
		red=MIN(red,REDMIN);
		red=MAX(red,REDMAX);
		h *= red;
		reduct=1;
	}
	xx=xnew;
	hdid=h;
	first=0;
	wrkmin=1.0e35;
	for (kk=0;kk<=km;kk++) {
		fact=MAX(err[kk],SCALMX);
		work=fact*a[kk+1];
		if (work < wrkmin) {
			scale=fact;
			wrkmin=work;
			kopt=kk+1;
		}
	}
	hnext=h/scale;
	if (kopt >= k && kopt != kmax && !reduct) {
		fact=MAX(scale/alf[kopt-1][kopt],SCALMX);
		if (a[kopt+1]*fact <= wrkmin) {
			hnext=h/fact;
			kopt++;
		}
	}
	delete d_p;
	delete x_p;
}
