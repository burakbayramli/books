#include <cmath>
#include "nr.h"
using namespace std;

bool NR::zbrac(DP func(const DP), DP &x1, DP &x2)
{
	const int NTRY=50;
	const DP FACTOR=1.6;
	int j;
	DP f1,f2;

	if (x1 == x2) nrerror("Bad initial range in zbrac");
	f1=func(x1);
	f2=func(x2);
	for (j=0;j<NTRY;j++) {
		if (f1*f2 < 0.0) return true;
		if (fabs(f1) < fabs(f2))
			f1=func(x1 += FACTOR*(x1-x2));
		else
			f2=func(x2 += FACTOR*(x2-x1));
	}
	return false;
}
