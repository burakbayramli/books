#include <cmath>
#include "nr.h"
using namespace std;

void NR::cosft2(Vec_IO_DP &y, const int isign)
{
	const DP PI=3.141592653589793238;
	int i;
	DP sum,sum1,y1,y2,ytemp,theta,wi=0.0,wi1,wpi,wpr,wr=1.0,wr1,wtemp;

	int n=y.size();
	theta=0.5*PI/n;
	wr1=cos(theta);
	wi1=sin(theta);
	wpr = -2.0*wi1*wi1;
	wpi=sin(2.0*theta);
	if (isign == 1) {
		for (i=0;i<n/2;i++) {
			y1=0.5*(y[i]+y[n-1-i]);
			y2=wi1*(y[i]-y[n-1-i]);
			y[i]=y1+y2;
			y[n-1-i]=y1-y2;
			wr1=(wtemp=wr1)*wpr-wi1*wpi+wr1;
			wi1=wi1*wpr+wtemp*wpi+wi1;
		}
		realft(y,1);
		for (i=2;i<n;i+=2) {
			wr=(wtemp=wr)*wpr-wi*wpi+wr;
			wi=wi*wpr+wtemp*wpi+wi;
			y1=y[i]*wr-y[i+1]*wi;
			y2=y[i+1]*wr+y[i]*wi;
			y[i]=y1;
			y[i+1]=y2;
		}
		sum=0.5*y[1];
		for (i=n-1;i>0;i-=2) {
			sum1=sum;
			sum += y[i];
			y[i]=sum1;
		}
	} else if (isign == -1) {
		ytemp=y[n-1];
		for (i=n-1;i>2;i-=2)
			y[i]=y[i-2]-y[i];
		y[1]=2.0*ytemp;
		for (i=2;i<n;i+=2) {
			wr=(wtemp=wr)*wpr-wi*wpi+wr;
			wi=wi*wpr+wtemp*wpi+wi;
			y1=y[i]*wr+y[i+1]*wi;
			y2=y[i+1]*wr-y[i]*wi;
			y[i]=y1;
			y[i+1]=y2;
		}
		realft(y,-1);
		for (i=0;i<n/2;i++) {
			y1=y[i]+y[n-1-i];
			y2=(0.5/wi1)*(y[i]-y[n-1-i]);
			y[i]=0.5*(y1+y2);
			y[n-1-i]=0.5*(y1-y2);
			wr1=(wtemp=wr1)*wpr-wi1*wpi+wr1;
			wi1=wi1*wpr+wtemp*wpi+wi1;
		}
	}
}
