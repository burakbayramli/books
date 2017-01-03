#include "nr.h"

namespace {
	void sift_down(Vec_IO_DP &ra, const int l, const int r)
	{
		int j,jold;
		DP a;

		a=ra[l];
		jold=l;
		j=l+1;
		while (j <= r) {
			if (j < r && ra[j] < ra[j+1]) j++;
			if (a >= ra[j]) break;
			ra[jold]=ra[j];
			jold=j;
			j=2*j+1;
		}
		ra[jold]=a;
	}
}

void NR::hpsort(Vec_IO_DP &ra)
{
	int i;

	int n=ra.size();
	for (i=n/2-1; i>=0; i--)
		sift_down(ra,i,n-1);
	for (i=n-1; i>0; i--) {
		SWAP(ra[0],ra[i]);
		sift_down(ra,0,i-1);
	}
}
