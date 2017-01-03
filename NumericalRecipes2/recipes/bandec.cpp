#include <cmath>
#include "nr.h"
using namespace std;

void NR::bandec(Mat_IO_DP &a, const int m1, const int m2, Mat_O_DP &al,
	Vec_O_INT &indx, DP &d)
{
	const DP TINY=1.0e-20;
	int i,j,k,l,mm;
	DP dum;

	int n=a.nrows();
	mm=m1+m2+1;
	l=m1;
	for (i=0;i<m1;i++) {
		for (j=m1-i;j<mm;j++) a[i][j-l]=a[i][j];
		l--;
		for (j=mm-l-1;j<mm;j++) a[i][j]=0.0;
	}
	d=1.0;
	l=m1;
	for (k=0;k<n;k++) {
		dum=a[k][0];
		i=k;
		if (l<n) l++;
		for (j=k+1;j<l;j++) {
			if (fabs(a[j][0]) > fabs(dum)) {
				dum=a[j][0];
				i=j;
			}
		}
		indx[k]=i+1;
		if (dum == 0.0) a[k][0]=TINY;
		if (i != k) {
			d = -d;
			for (j=0;j<mm;j++) SWAP(a[k][j],a[i][j]);
		}
		for (i=k+1;i<l;i++) {
			dum=a[i][0]/a[k][0];
			al[k][i-k-1]=dum;
			for (j=1;j<mm;j++) a[i][j-1]=a[i][j]-dum*a[k][j];
			a[i][mm-1]=0.0;
		}
	}
}
