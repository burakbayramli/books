#include <cmath>
#include "nr.h"
using namespace std;

void NR::memcof(Vec_I_DP &data, DP &xms, Vec_O_DP &d)
{
	int k,j,i;
	DP p=0.0;

	int n=data.size();
	int m=d.size();
	Vec_DP wk1(n),wk2(n),wkm(m);
	for (j=0;j<n;j++) p += SQR(data[j]);
	xms=p/n;
	wk1[0]=data[0];
	wk2[n-2]=data[n-1];
	for (j=1;j<n-1;j++) {
		wk1[j]=data[j];
		wk2[j-1]=data[j];
	}
	for (k=0;k<m;k++) {
		DP num=0.0,denom=0.0;
		for (j=0;j<(n-k-1);j++) {
			num += (wk1[j]*wk2[j]);
			denom += (SQR(wk1[j])+SQR(wk2[j]));
		}
		d[k]=2.0*num/denom;
		xms *= (1.0-SQR(d[k]));
		for (i=0;i<k;i++)
			d[i]=wkm[i]-d[k]*wkm[k-1-i];
		if (k == m-1)
			return;
		for (i=0;i<=k;i++) wkm[i]=d[i];
		for (j=0;j<(n-k-2);j++) {
			wk1[j] -= (wkm[k]*wk2[j]);
			wk2[j]=wk2[j+1]-wkm[k]*wk1[j+1];
		}
	}
	nrerror("never get here in memcof.");
}
