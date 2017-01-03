#include "nr.h"

void NR::fpoly(const DP x, Vec_O_DP &p)
{
	int j;

	int np=p.size();
	p[0]=1.0;
	for (j=1;j<np;j++) p[j]=p[j-1]*x;
}
