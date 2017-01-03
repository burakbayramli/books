#include <cmath>
#include "nr.h"
using namespace std;

void NR::stiff(Vec_IO_DP &y, Vec_IO_DP &dydx, DP &x, const DP htry,
	const DP eps, Vec_I_DP &yscal, DP &hdid, DP &hnext,
	void derivs(const DP, Vec_I_DP &, Vec_O_DP &))
{
	const DP SAFETY=0.9,GROW=1.5,PGROW= -0.25,SHRNK=0.5;
	const DP PSHRNK=(-1.0/3.0),ERRCON=0.1296;
	const int MAXTRY=40;
	const DP GAM=1.0/2.0,A21=2.0,A31=48.0/25.0,A32=6.0/25.0,C21= -8.0,
		C31=372.0/25.0,C32=12.0/5.0,C41=(-112.0/125.0),
		C42=(-54.0/125.0),C43=(-2.0/5.0),B1=19.0/9.0,B2=1.0/2.0,
		B3=25.0/108.0,B4=125.0/108.0,E1=17.0/54.0,E2=7.0/36.0,E3=0.0,
		E4=125.0/108.0,C1X=1.0/2.0,C2X=(-3.0/2.0),C3X=(121.0/50.0),
		C4X=(29.0/250.0),A2X=1.0,A3X=3.0/5.0;
	int i,j,jtry;
	DP d,errmax,h,xsav;

	int n=y.size();
	Mat_DP a(n,n),dfdy(n,n);
	Vec_INT indx(n);
	Vec_DP dfdx(n),dysav(n),err(n),ysav(n),g1(n),g2(n),g3(n),g4(n);
	xsav=x;
	for (i=0;i<n;i++) {
		ysav[i]=y[i];
		dysav[i]=dydx[i];
	}
	jacobn_s(xsav,ysav,dfdx,dfdy);
	h=htry;
	for (jtry=0;jtry<MAXTRY;jtry++) {
		for (i=0;i<n;i++) {
			for (j=0;j<n;j++) a[i][j] = -dfdy[i][j];
			a[i][i] += 1.0/(GAM*h);
		}
		ludcmp(a,indx,d);
		for (i=0;i<n;i++)
			g1[i]=dysav[i]+h*C1X*dfdx[i];
		lubksb(a,indx,g1);
		for (i=0;i<n;i++)
			y[i]=ysav[i]+A21*g1[i];
		x=xsav+A2X*h;
		derivs(x,y,dydx);
		for (i=0;i<n;i++)
			g2[i]=dydx[i]+h*C2X*dfdx[i]+C21*g1[i]/h;
		lubksb(a,indx,g2);
		for (i=0;i<n;i++)
			y[i]=ysav[i]+A31*g1[i]+A32*g2[i];
		x=xsav+A3X*h;
		derivs(x,y,dydx);
		for (i=0;i<n;i++)
			g3[i]=dydx[i]+h*C3X*dfdx[i]+(C31*g1[i]+C32*g2[i])/h;
		lubksb(a,indx,g3);
		for (i=0;i<n;i++)
			g4[i]=dydx[i]+h*C4X*dfdx[i]+(C41*g1[i]+C42*g2[i]+C43*g3[i])/h;
		lubksb(a,indx,g4);
		for (i=0;i<n;i++) {
			y[i]=ysav[i]+B1*g1[i]+B2*g2[i]+B3*g3[i]+B4*g4[i];
			err[i]=E1*g1[i]+E2*g2[i]+E3*g3[i]+E4*g4[i];
		}
		x=xsav+h;
		if (x == xsav) nrerror("stepsize not significant in stiff");
		errmax=0.0;
		for (i=0;i<n;i++) errmax=MAX(errmax,fabs(err[i]/yscal[i]));
		errmax /= eps;
		if (errmax <= 1.0) {
			hdid=h;
			hnext=(errmax > ERRCON ? SAFETY*h*pow(errmax,PGROW) : GROW*h);
			return;
		} else {
			hnext=SAFETY*h*pow(errmax,PSHRNK);
			h=(h >= 0.0 ? MAX(hnext,SHRNK*h) : MIN(hnext,SHRNK*h));
		}
	}
	nrerror("exceeded MAXTRY in stiff");
}
