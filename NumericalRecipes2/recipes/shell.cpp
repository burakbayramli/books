#include "nr.h"

void NR::shell(const int m, Vec_IO_DP &a)
{
	int i,j,inc;
	DP v;

	inc=1;
	do {
		inc *= 3;
		inc++;
	} while (inc <= m);
	do {
		inc /= 3;
		for (i=inc;i<m;i++) {
			v=a[i];
			j=i;
			while (a[j-inc] > v) {
				a[j]=a[j-inc];
				j -= inc;
				if (j < inc) break;
			}
			a[j]=v;
		}
	} while (inc > 1);
}
