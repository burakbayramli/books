#include "nr.h"

void NR::simplx(Mat_IO_DP &a, const int m1, const int m2, const int m3,
	int &icase, Vec_O_INT &izrov, Vec_O_INT &iposv)
{
	const DP EPS=1.0e-14;
	int i,k,ip,is,kh,kp,nl1;
	DP q1,bmax;

	int m=a.nrows()-2;
	int n=a.ncols()-1;
	if (m != (m1+m2+m3)) nrerror("Bad input constraint counts in simplx");
	Vec_INT l1(n+1),l3(m);
	nl1=n;
	for (k=0;k<n;k++) {
		l1[k]=k+1;
		izrov[k]=k;
	}
	for (i=1;i<=m;i++) {
		if (a[i][0] < 0.0) nrerror("Bad input tableau in simplx");
		iposv[i-1]=n+i-1;
	}
	if (m2+m3 != 0) {
		for (i=0;i<m2;i++) l3[i]=1;
		for (k=0;k<(n+1);k++) {
			q1=0.0;
			for (i=m1+1;i<m+1;i++) q1 += a[i][k];
			a[m+1][k] = -q1;
		}
		for (;;) {
			simp1(a,m+1,l1,nl1,0,kp,bmax);
			if (bmax <= EPS && a[m+1][0] < -EPS) {
				icase = -1;
				return;
			} else if (bmax <= EPS && a[m+1][0] <= EPS) {
				for (ip=m1+m2+1;ip<m+1;ip++) {
					if (iposv[ip-1] == (ip+n-1)) {
						simp1(a,ip,l1,nl1,1,kp,bmax);
						if (bmax > EPS)
							goto one;
					}
				}
				for (i=m1+1;i<=m1+m2;i++)
					if (l3[i-m1-1] == 1)
						for (k=0;k<n+1;k++)
							a[i][k]= -a[i][k];
				break;
			}
			simp2(a,m,n,ip,kp);
			if (ip == 0) {
				icase = -1;
				return;
			}
	one:	simp3(a,m+1,n,ip,kp);
			if (iposv[ip-1] >= (n+m1+m2)) {
				for (k=0;k<nl1;k++)
					if (l1[k] == kp) break;
				--nl1;
				for (is=k;is<nl1;is++) l1[is]=l1[is+1];
			} else {
				kh=iposv[ip-1]-m1-n+1;
				if (kh >= 1 && l3[kh-1]) {
					l3[kh-1]=0;
					++a[m+1][kp];
					for (i=0;i<m+2;i++)
						a[i][kp]= -a[i][kp];
				}
			}
			SWAP(izrov[kp-1],iposv[ip-1]);
		}
	}
	for (;;) {
		simp1(a,0,l1,nl1,0,kp,bmax);
		if (bmax <= EPS) {
			icase=0;
			return;
		}
		simp2(a,m,n,ip,kp);
		if (ip == 0) {
			icase=1;
			return;
		}
		simp3(a,m,n,ip,kp);
		SWAP(izrov[kp-1],iposv[ip-1]);
	}
}
