#include "nr.h"

DP NR::bessk(const int n, const DP x)
{
	int j;
	DP bk,bkm,bkp,tox;

	if (n < 2) nrerror("Index n less than 2 in bessk");
	tox=2.0/x;
	bkm=bessk0(x);
	bk=bessk1(x);
	for (j=1;j<n;j++) {
		bkp=bkm+j*tox*bk;
		bkm=bk;
		bk=bkp;
	}
	return bk;
}
