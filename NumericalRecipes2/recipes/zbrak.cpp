#include "nr.h"

void NR::zbrak(DP fx(const DP), const DP x1, const DP x2, const int n,
	Vec_O_DP &xb1, Vec_O_DP &xb2, int &nroot)
{
	int i;
	DP x,fp,fc,dx;

	int nb=xb1.size();
	nroot=0;
	dx=(x2-x1)/n;
	fp=fx(x=x1);
	for (i=0;i<n;i++) {
		fc=fx(x += dx);
		if (fc*fp <= 0.0) {
			xb1[nroot]=x-dx;
			xb2[nroot++]=x;
			if(nroot == nb) return;
		}
		fp=fc;
	}
}
