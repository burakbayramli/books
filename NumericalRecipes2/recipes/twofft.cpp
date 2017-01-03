#include "nr.h"

void NR::twofft(Vec_I_DP &data1, Vec_I_DP &data2, Vec_O_DP &fft1,
	Vec_O_DP &fft2)
{
	int nn3,nn2,jj,j;
	DP rep,rem,aip,aim;

	int n=data1.size();
	nn3=1+(nn2=n+n);
	for (j=0,jj=0;j<n;j++,jj+=2) {
		fft1[jj]=data1[j];
		fft1[jj+1]=data2[j];
	}
	four1(fft1,1);
	fft2[0]=fft1[1];
	fft1[1]=fft2[1]=0.0;
	for (j=2;j<n+1;j+=2) {
		rep=0.5*(fft1[j]+fft1[nn2-j]);
		rem=0.5*(fft1[j]-fft1[nn2-j]);
		aip=0.5*(fft1[j+1]+fft1[nn3-j]);
		aim=0.5*(fft1[j+1]-fft1[nn3-j]);
		fft1[j]=rep;
		fft1[j+1]=aim;
		fft1[nn2-j]=rep;
		fft1[nn3-j]= -aim;
		fft2[j]=aip;
		fft2[j+1]= -rem;
		fft2[nn2-j]=aip;
		fft2[nn3-j]=rem;
	}
}
