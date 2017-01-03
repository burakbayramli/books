#include "nr.h"

void NR::rank(Vec_I_INT &indx, Vec_O_INT &irank)
{
	int j;

	int n=indx.size();
	for (j=0;j<n;j++) irank[indx[j]]=j;
}
