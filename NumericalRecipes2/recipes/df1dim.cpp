#include "nr.h"

extern int ncom;
extern DP (*nrfunc)(Vec_I_DP &);
extern void (*nrdfun)(Vec_I_DP &, Vec_O_DP &);
extern Vec_DP *pcom_p,*xicom_p;

DP NR::df1dim(const DP x)
{
	int j;
	DP df1=0.0;
	Vec_DP xt(ncom),df(ncom);

	Vec_DP &pcom=*pcom_p,&xicom=*xicom_p;
	for (j=0;j<ncom;j++) xt[j]=pcom[j]+x*xicom[j];
	nrdfun(xt,df);
	for (j=0;j<ncom;j++) df1 += df[j]*xicom[j];
	return df1;
}
