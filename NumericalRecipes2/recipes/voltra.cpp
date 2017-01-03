#include "nr.h"

void NR::voltra(const DP t0, const DP h, Vec_O_DP &t, Mat_O_DP &f,
	DP g(const int, const DP),
	DP ak(const int, const int, const DP, const DP))
{
	int i,j,k,l;
	DP d,sum;

	int m=f.nrows();
	int n=f.ncols();
	Vec_INT indx(m);
	Vec_DP b(m);
	Mat_DP a(m,m);
	t[0]=t0;
	for (k=0;k<m;k++) f[k][0]=g(k,t[0]);
	for (i=1;i<n;i++) {
		t[i]=t[i-1]+h;
		for (k=0;k<m;k++) {
			sum=g(k,t[i]);
			for (l=0;l<m;l++) {
				sum += 0.5*h*ak(k,l,t[i],t[0])*f[l][0];
				for (j=1;j<i;j++)
					sum += h*ak(k,l,t[i],t[j])*f[l][j];
				if (k == l)
					a[k][l]=1.0-0.5*h*ak(k,l,t[i],t[i]);
				else
					a[k][l] = -0.5*h*ak(k,l,t[i],t[i]);
			}
			b[k]=sum;
		}
		ludcmp(a,indx,d);
		lubksb(a,indx,b);
		for (k=0;k<m;k++) f[k][i]=b[k];
	}
}
