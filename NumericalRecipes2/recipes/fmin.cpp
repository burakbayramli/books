#include "nr.h"

extern Vec_DP *fvec_p;
extern void (*nrfuncv)(Vec_I_DP &v, Vec_O_DP &f);

DP NR::fmin(Vec_I_DP &x)
{
	int i;
	DP sum;

	Vec_DP &fvec=*fvec_p;
	nrfuncv(x,fvec);
	int n=x.size();
	for (sum=0.0,i=0;i<n;i++) sum += SQR(fvec[i]);
	return 0.5*sum;
}
