#include "nr.h"

DP NR::ratval(const DP x, Vec_I_DP &cof, const int mm, const int kk)
{
	int j;
	DP sumd,sumn;

	for (sumn=cof[mm],j=mm-1;j>=0;j--) sumn=sumn*x+cof[j];
	for (sumd=0.0,j=mm+kk;j>mm;j--) sumd=(sumd+cof[j])*x;
	return sumn/(1.0+sumd);
}
