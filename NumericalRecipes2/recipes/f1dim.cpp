#include "nr.h"

extern int ncom;
extern DP (*nrfunc)(Vec_I_DP &);
extern Vec_DP *pcom_p,*xicom_p;

DP NR::f1dim(const DP x)
{
	int j;

	Vec_DP xt(ncom);
	Vec_DP &pcom=*pcom_p,&xicom=*xicom_p;
	for (j=0;j<ncom;j++)
		xt[j]=pcom[j]+x*xicom[j];
	return nrfunc(xt);
}
