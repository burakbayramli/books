#include "nr.h"

extern int n2;
extern DP x1,x2,xf;

int kmax,kount;
DP dxsav;
Mat_DP *yp_p;
Vec_DP *xp_p;

void derivs(const DP x, Vec_I_DP &y, Vec_O_DP &dydx);
void load1(const DP x1, Vec_I_DP &v1, Vec_O_DP &y);
void load2(const DP x2, Vec_I_DP &v2, Vec_O_DP &y);
void score(const DP xf, Vec_I_DP &y, Vec_O_DP &f);

void NR::shootf(Vec_I_DP &v, Vec_O_DP &f)
{
	const DP EPS=1.0e-14;
	int i,nbad,nok;
	DP h1,hmin=0.0;

	int nvar=v.size();
	Vec_DP f1(nvar),f2(nvar),y(nvar);
	Vec_DP v2(&v[n2],nvar-n2);
	kmax=0;
	h1=(x2-x1)/100.0;
	load1(x1,v,y);
	odeint(y,x1,xf,EPS,h1,hmin,nok,nbad,derivs,rkqs);
	score(xf,y,f1);
	load2(x2,v2,y);
	odeint(y,x2,xf,EPS,h1,hmin,nok,nbad,derivs,rkqs);
	score(xf,y,f2);
	for (i=0;i<nvar;i++) f[i]=f1[i]-f2[i];
}
