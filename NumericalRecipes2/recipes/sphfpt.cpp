#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

int m,n;
DP c2,dx,gmma;

int n2;
DP x1,x2,xf;

int main(void)	// Program sphfpt
{
	const int N1=2,N2=1,NTOT=N1+N2;
	const DP DXX=1.0e-8;
	bool check;
	int i;
	DP q1;
	Vec_DP v(NTOT);

	n2=N2;
	dx=DXX;
	for (;;) {
		cout << endl << "input m,n,c-squared (n >= m, c=999 to end)" << endl;
		cin >> m >> n >> c2;
		if (c2 == 999) break;
		if (n < m || m < 0) continue;
		gmma=1.0;
		q1=n;
		for (i=1;i<=m;i++) gmma *= -0.5*(n+i)*(q1--/i);
		v[0]=n*(n+1)-m*(m+1)+c2/2.0;
		v[2]=v[0];
		v[1]=gmma*(1.0-(v[2]-c2)*dx/(2*(m+1)));
		x1= -1.0+dx;
		x2=1.0-dx;
		xf=0.0;
		NR::newt(v,check,NR::shootf);
		if (check) {
			cout << "shootf failed; bad initial guess" << endl;
		} else {
			cout << "    " << "mu(m,n)" << endl;
			cout << fixed << setprecision(6);
			cout << setw(12) << v[0] << endl;
		}
	}
	return 0;
}

void load1(const DP x1, Vec_I_DP &v1, Vec_O_DP &y)
{
	DP y1 = ((n-m & 1) != 0 ? -gmma : gmma);
	y[2]=v1[0];
	y[1] = -(y[2]-c2)*y1/(2*(m+1));
	y[0]=y1+y[1]*dx;
}

void load2(const DP x2, Vec_I_DP &v2, Vec_O_DP &y)
{
	y[2]=v2[1];
	y[0]=v2[0];
	y[1]=(y[2]-c2)*y[0]/(2*(m+1));
}

void score(const DP xf, Vec_I_DP &y, Vec_O_DP &f)
{
	int i;

	for (i=0;i<3;i++) f[i]=y[i];
}

void derivs(const DP x, Vec_I_DP &y, Vec_O_DP &dydx)
{
	dydx[0]=y[1];
	dydx[1]=(2.0*x*(m+1.0)*y[1]-(y[2]-c2*x*x)*y[0])/(1.0-x*x);
	dydx[2]=0.0;
}
