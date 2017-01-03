#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

int m,n;
DP c2,dx,gmma;

int nvar;
DP x1,x2;

int main(void)	// Program sphoot
{
	const int N2=1;
	bool check;
	int i;
	DP q1;
	Vec_DP v(N2);

	dx=1.0e-8;
	nvar=3;
	for (;;) {
		cout << endl << "input m,n,c-squared (999 to end)" << endl;
		cin >> m >> n >> c2;
		if (c2 == 999) break;
		if (n < m || m < 0) continue;
		gmma=1.0;
		q1=n;
		for (i=1;i<=m;i++) gmma *= -0.5*(n+i)*(q1--/i);
		v[0]=n*(n+1)-m*(m+1)+c2/2.0;
		x1= -1.0+dx;
		x2=0.0;
		NR::newt(v,check,NR::shoot);
		if (check) {
			cout << "shoot failed; bad initial guess" << endl;
		} else {
			cout << "    " << "mu(m,n)" << endl;
			cout << fixed << setprecision(6);
			cout << setw(12) << v[0] << endl;
		}
	}
	return 0;
}

void load(const DP x1, Vec_I_DP &v, Vec_O_DP &y)
{
	DP y1 = ((n-m & 1) != 0 ? -gmma : gmma);
	y[2]=v[0];
	y[1] = -(y[2]-c2)*y1/(2*(m+1));
	y[0]=y1+y[1]*dx;
}

void score(const DP xf, Vec_I_DP &y, Vec_O_DP &f)
{
	f[0]=((n-m & 1) != 0 ? y[0] : y[1]);
}

void derivs(const DP x, Vec_I_DP &y, Vec_O_DP &dydx)
{
	dydx[0]=y[1];
	dydx[1]=(2.0*x*(m+1.0)*y[1]-(y[2]-c2*x*x)*y[0])/(1.0-x*x);
	dydx[2]=0.0;
}
