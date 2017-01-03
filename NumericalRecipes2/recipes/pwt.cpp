#include "nr.h"

extern wavefilt *wfilt_p;

void NR::pwt(Vec_IO_DP &a, const int n, const int isign)
{
	DP ai,ai1;
	int i,ii,j,jf,jr,k,n1,ni,nj,nh,nmod;

	if (n < 4) return;
	wavefilt &wfilt=*wfilt_p;
	Vec_DP wksp(n);
	nmod=wfilt.ncof*n;
	n1=n-1;
	nh=n >> 1;
	for (j=0;j<n;j++) wksp[j]=0.0;
	if (isign >= 0) {
		for (ii=0,i=0;i<n;i+=2,ii++) {
			ni=i+1+nmod+wfilt.ioff;
			nj=i+1+nmod+wfilt.joff;
			for (k=0;k<wfilt.ncof;k++) {
				jf=n1 & (ni+k+1);
				jr=n1 & (nj+k+1);
				wksp[ii] += wfilt.cc[k]*a[jf];
				wksp[ii+nh] += wfilt.cr[k]*a[jr];
			}
		}
	} else {
		for (ii=0,i=0;i<n;i+=2,ii++) {
			ai=a[ii];
			ai1=a[ii+nh];
			ni=i+1+nmod+wfilt.ioff;
			nj=i+1+nmod+wfilt.joff;
			for (k=0;k<wfilt.ncof;k++) {
				jf=n1 & (ni+k+1);
				jr=n1 & (nj+k+1);
				wksp[jf] += wfilt.cc[k]*ai;
				wksp[jr] += wfilt.cr[k]*ai1;
			}
		}
	}
	for (j=0;j<n;j++) a[j]=wksp[j];
}
