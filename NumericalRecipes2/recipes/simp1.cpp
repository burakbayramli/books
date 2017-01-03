#include <cmath>
#include "nr.h"
using namespace std;

void NR::simp1(Mat_I_DP &a, const int mm, Vec_I_INT &ll, const int nll,
	const int iabf, int &kp, DP &bmax)
{
	int k;
	DP test;

	if (nll <= 0)
		bmax=0.0;
	else {
		kp=ll[0];
		bmax=a[mm][kp];
		for (k=1;k<nll;k++) {
			if (iabf == 0)
				test=a[mm][ll[k]]-bmax;
			else
				test=fabs(a[mm][ll[k]])-fabs(bmax);
			if (test > 0.0) {
				bmax=a[mm][ll[k]];
				kp=ll[k];
			}
		}
	}
}
