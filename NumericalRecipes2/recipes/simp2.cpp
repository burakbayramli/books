#include "nr.h"

void NR::simp2(Mat_I_DP &a, const int m, const int n, int &ip, const int kp)
{
	const DP EPS=1.0e-14;
	int k,i;
	DP qp,q0,q,q1;

	ip=0;
	for (i=0;i<m;i++)
		if (a[i+1][kp] < -EPS) break;
	if (i+1>m) return;
	q1 = -a[i+1][0]/a[i+1][kp];
	ip=i+1;
	for (i=ip;i<m;i++) {
		if (a[i+1][kp] < -EPS) {
			q = -a[i+1][0]/a[i+1][kp];
			if (q < q1) {
				ip=i+1;
				q1=q;
			} else if (q == q1) {
				for (k=0;k<n;k++) {
					qp = -a[ip][k+1]/a[ip][kp];
					q0 = -a[i][k+1]/a[i][kp];
					if (q0 != qp) break;
				}
				if (q0 < qp) ip=i+1;
			}
		}
	}
}
