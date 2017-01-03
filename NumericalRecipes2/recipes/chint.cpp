#include "nr.h"

void NR::chint(const DP a, const DP b, Vec_I_DP &c, Vec_O_DP &cint, const int n)
{
	int j;
	DP sum=0.0,fac=1.0,con;

	con=0.25*(b-a);
	for (j=1;j<n-1;j++) {
		cint[j]=con*(c[j-1]-c[j+1])/j;
		sum += fac*cint[j];
		fac = -fac;
	}
	cint[n-1]=con*c[n-2]/(n-1);
	sum += fac*cint[n-1];
	cint[0]=2.0*sum;
}
