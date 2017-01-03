#include "nr.h"

void NR::simp3(Mat_IO_DP &a, const int i1, const int k1, const int ip,
	const int kp)
{
	int ii,kk;
	DP piv;

	piv=1.0/a[ip][kp];
	for (ii=0;ii<i1+1;ii++)
		if (ii != ip) {
			a[ii][kp] *= piv;
			for (kk=0;kk<k1+1;kk++)
				if (kk != kp)
					a[ii][kk] -= a[ip][kk]*a[ii][kp];
		}
	for (kk=0;kk<k1+1;kk++)
		if (kk != kp) a[ip][kk] *= -piv;
	a[ip][kp]=piv;
}
