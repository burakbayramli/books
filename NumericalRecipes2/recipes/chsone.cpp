#include "nr.h"

void NR::chsone(Vec_I_DP &bins, Vec_I_DP &ebins, const int knstrn, DP &df,
	DP &chsq, DP &prob)
{
	int j;
	DP temp;

	int nbins=bins.size();
	df=nbins-knstrn;
	chsq=0.0;
	for (j=0;j<nbins;j++) {
		if (ebins[j] <= 0.0) nrerror("Bad expected number in chsone");
		temp=bins[j]-ebins[j];
		chsq += temp*temp/ebins[j];
	}
	prob=gammq(0.5*df,0.5*chsq);
}
