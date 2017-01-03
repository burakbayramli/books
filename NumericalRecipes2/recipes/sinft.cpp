#include <cmath>
#include "nr.h"
using namespace std;

void NR::sinft(Vec_IO_DP &y)
{
	int j;
	DP sum,y1,y2,theta,wi=0.0,wr=1.0,wpi,wpr,wtemp;

	int n=y.size();
	theta=3.141592653589793238/DP(n);
	wtemp=sin(0.5*theta);
	wpr= -2.0*wtemp*wtemp;
	wpi=sin(theta);
	y[0]=0.0;
	for (j=1;j<(n>>1)+1;j++) {
		wr=(wtemp=wr)*wpr-wi*wpi+wr;
		wi=wi*wpr+wtemp*wpi+wi;
		y1=wi*(y[j]+y[n-j]);
		y2=0.5*(y[j]-y[n-j]);
		y[j]=y1+y2;
		y[n-j]=y1-y2;
	}
	realft(y,1);
	y[0]*=0.5;
	sum=y[1]=0.0;
	for (j=0;j<n-1;j+=2) {
		sum += y[j];
		y[j]=y[j+1];
		y[j+1]=sum;
	}
}
