#include "nr.h"

void NR::locate(Vec_I_DP &xx, const DP x, int &j)
{
	int ju,jm,jl;
	bool ascnd;

	int n=xx.size();
	jl=-1;
	ju=n;
	ascnd=(xx[n-1] >= xx[0]);
	while (ju-jl > 1) {
		jm=(ju+jl) >> 1;
		if (x >= xx[jm] == ascnd)
			jl=jm;
		else
			ju=jm;
	}
	if (x == xx[0]) j=0;
	else if (x == xx[n-1]) j=n-2;
	else j=jl;
}
