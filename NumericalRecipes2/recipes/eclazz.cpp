#include "nr.h"

void NR::eclazz(Vec_O_INT &nf, bool equiv(const int, const int))
{
	int kk,jj;

	int n=nf.size();
	nf[0]=0;
	for (jj=1;jj<n;jj++) {
		nf[jj]=jj;
		for (kk=0;kk<jj;kk++) {
			nf[kk]=nf[nf[kk]];
			if (equiv(jj+1,kk+1)) nf[nf[nf[kk]]]=jj;
		}
	}
	for (jj=0;jj<n;jj++) nf[jj]=nf[nf[jj]];
}
