#include "nr.h"

extern int nvar;
extern DP x1,x2;

int kmax,kount;
DP dxsav;
Vec_DP *xp_p;
Mat_DP *yp_p;

void derivs(const DP x, Vec_I_DP &y, Vec_O_DP &dydx);
void load(const DP x1, Vec_I_DP &v, Vec_O_DP &y);
void score(const DP xf, Vec_I_DP &y, Vec_O_DP &f);

void NR::shoot(Vec_I_DP &v, Vec_O_DP &f)
{
	const DP EPS=1.0e-14;
	int nbad,nok;
	DP h1,hmin=0.0;

	Vec_DP y(nvar);
	kmax=0;
	h1=(x2-x1)/100.0;
	load(x1,v,y);
	odeint(y,x1,x2,EPS,h1,hmin,nok,nbad,derivs,rkqs);
	score(x2,y,f);
}
