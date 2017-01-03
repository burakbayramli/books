#include <cmath>
#include "nr.h"
using namespace std;

void NR::cosft1(Vec_IO_DP &y)
{
	const DP PI=3.141592653589793238;
	int j;
	DP sum,y1,y2,theta,wi=0.0,wpi,wpr,wr=1.0,wtemp;

	int n=y.size()-1;
	Vec_DP yy(n);
	theta=PI/n;
	wtemp=sin(0.5*theta);
	wpr = -2.0*wtemp*wtemp;
	wpi=sin(theta);
	sum=0.5*(y[0]-y[n]);
	yy[0]=0.5*(y[0]+y[n]);
	for (j=1;j<n/2;j++) {
		wr=(wtemp=wr)*wpr-wi*wpi+wr;
		wi=wi*wpr+wtemp*wpi+wi;
		y1=0.5*(y[j]+y[n-j]);
		y2=(y[j]-y[n-j]);
		yy[j]=y1-wi*y2;
		yy[n-j]=y1+wi*y2;
		sum += wr*y2;
	}
	yy[n/2]=y[n/2];
	realft(yy,1);
	for (j=0;j<n;j++) y[j]=yy[j];
	y[n]=y[1];
	y[1]=sum;
	for (j=3;j<n;j+=2) {
		sum += y[j];
		y[j]=sum;
	}
}
