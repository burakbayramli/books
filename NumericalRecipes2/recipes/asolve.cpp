#include "nr.h"

extern Vec_INT *ija_p;
extern Vec_DP *sa_p;

void NR::asolve(Vec_I_DP &b, Vec_O_DP &x, const int itrnsp)
{
	int i;

	int n=b.size();
	for(i=0;i<n;i++) x[i]=((*sa_p)[i] != 0.0 ? b[i]/(*sa_p)[i] : b[i]);
}
