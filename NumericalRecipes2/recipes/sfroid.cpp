#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

const int M=40;
int mm,n,mpt=M+1;
DP h,c2=0.0,anorm;
Vec_DP *x_p;

int main(void)	// Program sfroid
{
	const int NE=3,NB=1,NYJ=NE,NYK=M+1;
	int i,itmax,k;
	DP conv,deriv,fac1,fac2,q1,slowc;
	Vec_INT indexv(NE);
	Vec_DP scalv(NE);
	Mat_DP y(NYJ,NYK);

	x_p=new Vec_DP(M+1);
	Vec_DP &x=*x_p;
	itmax=100;
	conv=1.0e-14;
	slowc=1.0;
	h=1.0/M;
	cout << endl << "Enter m n" << endl;
	cin >> mm >> n;
	if ((n+mm & 1) != 0) {
		indexv[0]=0;
		indexv[1]=1;
		indexv[2]=2;
	} else {
		indexv[0]=1;
		indexv[1]=0;
		indexv[2]=2;
	}
	anorm=1.0;
	if (mm != 0) {
		q1=n;
		for (i=1;i<=mm;i++) anorm = -0.5*anorm*(n+i)*(q1--/i);
	}
	for (k=0;k<M;k++) {
		x[k]=k*h;
		fac1=1.0-x[k]*x[k];
		fac2=exp((-mm/2.0)*log(fac1));
		y[0][k]=NR::plgndr(n,mm,x[k])*fac2;
		deriv = -((n-mm+1)*NR::plgndr(n+1,mm,x[k])-
			(n+1)*x[k]*NR::plgndr(n,mm,x[k]))/fac1;
		y[1][k]=mm*x[k]*y[0][k]/fac1+deriv*fac2;
		y[2][k]=n*(n+1)-mm*(mm+1);
	}
	x[M]=1.0;
	y[0][M]=anorm;
	y[2][M]=n*(n+1)-mm*(mm+1);
	y[1][M]=(y[2][M]-c2)*y[0][M]/(2.0*(mm+1.0));
	scalv[0]=fabs(anorm);
	scalv[1]=(y[1][M] > scalv[0] ? y[1][M] : scalv[0]);
	scalv[2]=(y[2][M] > 1.0 ? y[2][M] : 1.0);
	for (;;) {
		cout << endl << "Enter c**2 or 999 to end" << endl;
		cin >> c2;
		if (c2 == 999) {
			delete x_p;
			return 0;
		}
		NR::solvde(itmax,conv,slowc,scalv,indexv,NB,y);
		cout << endl << " m = " << setw(3) << mm;
		cout << "  n = " << setw(3) << n << "  c**2 = ";
		cout << fixed << setprecision(3) << setw(7) << c2;
		cout << " lamda = " << setprecision(6) << (y[2][0]+mm*(mm+1));
		cout << endl;
	}
}
