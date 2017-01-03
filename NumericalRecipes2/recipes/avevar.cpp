#include "nr.h"

void NR::avevar(Vec_I_DP &data, DP &ave, DP &var)
{
	DP s,ep;
	int j;

	int n=data.size();
	ave=0.0;
	for (j=0;j<n;j++) ave += data[j];
	ave /= n;
	var=ep=0.0;
	for (j=0;j<n;j++) {
		s=data[j]-ave;
		ep += s;
		var += s*s;
	}
	var=(var-ep*ep/n)/(n-1);
}
