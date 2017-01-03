#include "nr.h"

extern Vec_INT *ija_p;
extern Vec_DP *sa_p;

void NR::atimes(Vec_I_DP &x, Vec_O_DP &r, const int itrnsp)
{
	if (itrnsp) sprstx(*sa_p,*ija_p,x,r);
	else sprsax(*sa_p,*ija_p,x,r);
}
