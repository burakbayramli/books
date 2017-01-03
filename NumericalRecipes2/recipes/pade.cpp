#include <cmath>
#include "nr.h"
using namespace std;

void NR::pade(Vec_IO_DP &cof, DP &resid)
{
	const DP BIG=1.0e30;
	int j,k;
	DP d,rr,rrold,sum;

	int n=(cof.size()-1)/2;
	Mat_DP q(n,n),qlu(n,n);
	Vec_INT indx(n);
	Vec_DP x(n),y(n),z(n);
	for (j=0;j<n;j++) {
		y[j]=x[j]=cof[n+j+1];
		for (k=0;k<n;k++) {
			q[j][k]=cof[j-k+n];
			qlu[j][k]=q[j][k];
		}
	}
	ludcmp(qlu,indx,d);
	lubksb(qlu,indx,x);
	rr=BIG;
	do {
		rrold=rr;
		for (j=0;j<n;j++) z[j]=x[j];
		mprove(q,qlu,indx,y,x);
		for (rr=0.0,j=0;j<n;j++)
			rr += SQR(z[j]-x[j]);
	} while (rr < rrold);
	resid=sqrt(rrold);
	for (k=0;k<n;k++) {
		for (sum=cof[k+1],j=0;j<=k;j++)
			sum -= z[j]*cof[k-j];
		y[k]=sum;
	}
	for (j=0;j<n;j++) {
		cof[j+1]=y[j];
		cof[j+1+n] = -z[j];
	}
}
