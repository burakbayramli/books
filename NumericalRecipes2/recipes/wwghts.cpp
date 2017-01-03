#include "nr.h"

void NR::wwghts(Vec_O_DP &wghts, const DP h,
	void kermom(Vec_O_DP &w, const DP y))
{
	int j,k;
	Vec_DP wold(4);
	DP hh,hi,c,fac,a,b;

	int n=wghts.size();
	hh=h;
	hi=1.0/hh;
	for (j=0;j<n;j++) wghts[j]=0.0;
	if (n >= 4) {
		Vec_DP wold(4),wnew(4),w(4);
		kermom(wold,0.0);
		b=0.0;
		for (j=0;j<n-3;j++) {
			c=j;
			a=b;
			b=a+hh;
			if (j == n-4) b=(n-1)*hh;
			kermom(wnew,b);
			for (fac=1.0,k=0;k<4;k++,fac*=hi)
				w[k]=(wnew[k]-wold[k])*fac;
			wghts[j] += (((c+1.0)*(c+2.0)*(c+3.0)*w[0]
				-(11.0+c*(12.0+c*3.0))*w[1]+3.0*(c+2.0)*w[2]-w[3])/6.0);
			wghts[j+1] += ((-c*(c+2.0)*(c+3.0)*w[0]
				+(6.0+c*(10.0+c*3.0))*w[1]-(3.0*c+5.0)*w[2]+w[3])*0.5);
			wghts[j+2] += ((c*(c+1.0)*(c+3.0)*w[0]
				-(3.0+c*(8.0+c*3.0))*w[1]+(3.0*c+4.0)*w[2]-w[3])*0.5);
			wghts[j+3] += ((-c*(c+1.0)*(c+2.0)*w[0]
				+(2.0+c*(6.0+c*3.0))*w[1]-3.0*(c+1.0)*w[2]+w[3])/6.0);
			for (k=0;k<4;k++) wold[k]=wnew[k];
		}
	} else if (n == 3) {
		Vec_DP wold(3),wnew(3),w(3);
		kermom(wold,0.0);
		kermom(wnew,hh+hh);
		w[0]=wnew[0]-wold[0];
		w[1]=hi*(wnew[1]-wold[1]);
		w[2]=hi*hi*(wnew[2]-wold[2]);
		wghts[0]=w[0]-1.5*w[1]+0.5*w[2];
		wghts[1]=2.0*w[1]-w[2];
		wghts[2]=0.5*(w[2]-w[1]);
	} else if (n == 2) {
		Vec_DP wold(2),wnew(2),w(2);
		kermom(wold,0.0);
		kermom(wnew,hh);
		wghts[0]=wnew[0]-wold[0]-(wghts[1]=hi*(wnew[1]-wold[1]));
	}
}
