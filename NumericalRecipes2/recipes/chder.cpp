#include "nr.h"

void NR::chder(const DP a, const DP b, Vec_I_DP &c, Vec_O_DP &cder, const int n)
{
	int j;
	DP con;

	cder[n-1]=0.0;
	cder[n-2]=2*(n-1)*c[n-1];
	for (j=n-2;j>0;j--)
		cder[j-1]=cder[j+1]+2*j*c[j];
	con=2.0/(b-a);
	for (j=0;j<n;j++)
		cder[j] *= con;
}
