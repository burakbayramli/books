#include "nr.h"

void NR::chstwo(Vec_I_DP &bins1, Vec_I_DP &bins2, const int knstrn, DP &df,
	DP &chsq, DP &prob)
{
	int j;
	DP temp;

	int nbins=bins1.size();
	df=nbins-knstrn;
	chsq=0.0;
	for (j=0;j<nbins;j++)
		if (bins1[j] == 0.0 && bins2[j] == 0.0)
			--df;
		else {
			temp=bins1[j]-bins2[j];
			chsq += temp*temp/(bins1[j]+bins2[j]);
		}
	prob=gammq(0.5*df,0.5*chsq);
}
