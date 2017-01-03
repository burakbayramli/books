#include "nr.h"

extern int idum;

void NR::ranpt(Vec_O_DP &pt, Vec_I_DP &regn)
{
	int j;

	int n=pt.size();
	for (j=0;j<n;j++)
		pt[j]=regn[j]+(regn[n+j]-regn[j])*ran1(idum);
}
