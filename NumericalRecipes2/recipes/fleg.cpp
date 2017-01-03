#include "nr.h"

void NR::fleg(const DP x, Vec_O_DP &pl)
{
	int j;
	DP twox,f2,f1,d;

	int nl=pl.size();
	pl[0]=1.0;
	pl[1]=x;
	if (nl > 2) {
		twox=2.0*x;
		f2=x;
		d=1.0;
		for (j=2;j<nl;j++) {
			f1=d++;
			f2+=twox;
			pl[j]=(f2*pl[j-1]-f1*pl[j-2])/d;
		}
	}
}
