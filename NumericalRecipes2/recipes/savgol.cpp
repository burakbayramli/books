#include <cmath>
#include "nr.h"
using namespace std;

void NR::savgol(Vec_O_DP &c, const int np, const int nl, const int nr,
	const int ld, const int m)
{
	int j,k,imj,ipj,kk,mm;
	DP d,fac,sum;

	if (np < nl+nr+1 || nl < 0 || nr < 0 || ld > m || nl+nr < m)
		nrerror("bad args in savgol");
	Vec_INT indx(m+1);
	Mat_DP a(m+1,m+1);
	Vec_DP b(m+1);
	for (ipj=0;ipj<=(m << 1);ipj++) {
		sum=(ipj ? 0.0 : 1.0);
		for (k=1;k<=nr;k++) sum += pow(DP(k),DP(ipj));
		for (k=1;k<=nl;k++) sum += pow(DP(-k),DP(ipj));
		mm=MIN(ipj,2*m-ipj);
		for (imj = -mm;imj<=mm;imj+=2) a[(ipj+imj)/2][(ipj-imj)/2]=sum;
	}
	ludcmp(a,indx,d);
	for (j=0;j<m+1;j++) b[j]=0.0;
	b[ld]=1.0;
	lubksb(a,indx,b);
	for (kk=0;kk<np;kk++) c[kk]=0.0;
	for (k = -nl;k<=nr;k++) {
		sum=b[0];
		fac=1.0;
		for (mm=1;mm<=m;mm++) sum += b[mm]*(fac *= k);
		kk=(np-k) % np;
		c[kk]=sum;
	}
}
