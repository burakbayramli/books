#include <cmath>
#include "nr.h"
using namespace std;

extern DP dxsav;
extern int kmax,kount;
extern Vec_DP *xp_p;
extern Mat_DP *yp_p;

void NR::odeint(Vec_IO_DP &ystart, const DP x1, const DP x2, const DP eps,
	const DP h1, const DP hmin, int &nok, int &nbad,
	void derivs(const DP, Vec_I_DP &, Vec_O_DP &),
	void rkqs(Vec_IO_DP &, Vec_IO_DP &, DP &, const DP, const DP,
	Vec_I_DP &, DP &, DP &, void (*)(const DP, Vec_I_DP &, Vec_O_DP &)))
{
	const int MAXSTP=10000;
	const DP TINY=1.0e-30;
	int i,nstp;
	DP xsav,x,hnext,hdid,h;

	int nvar=ystart.size();
	Vec_DP yscal(nvar),y(nvar),dydx(nvar);
	Vec_DP &xp=*xp_p;
	Mat_DP &yp=*yp_p;
	x=x1;
	h=SIGN(h1,x2-x1);
	nok = nbad = kount = 0;
	for (i=0;i<nvar;i++) y[i]=ystart[i];
	if (kmax > 0) xsav=x-dxsav*2.0;
	for (nstp=0;nstp<MAXSTP;nstp++) {
		derivs(x,y,dydx);
		for (i=0;i<nvar;i++)
			yscal[i]=fabs(y[i])+fabs(dydx[i]*h)+TINY;
		if (kmax > 0 && kount < kmax-1 && fabs(x-xsav) > fabs(dxsav)) {
			for (i=0;i<nvar;i++) yp[i][kount]=y[i];
			xp[kount++]=x;
			xsav=x;
		}
		if ((x+h-x2)*(x+h-x1) > 0.0) h=x2-x;
		rkqs(y,dydx,x,h,eps,yscal,hdid,hnext,derivs);
		if (hdid == h) ++nok; else ++nbad;
		if ((x-x2)*(x2-x1) >= 0.0) {
			for (i=0;i<nvar;i++) ystart[i]=y[i];
			if (kmax != 0) {
				for (i=0;i<nvar;i++) yp[i][kount]=y[i];
				xp[kount++]=x;
			}
			return;
		}
		if (fabs(hnext) <= hmin) nrerror("Step size too small in odeint");
		h=hnext;
	}
	nrerror("Too many steps in routine odeint");
}
