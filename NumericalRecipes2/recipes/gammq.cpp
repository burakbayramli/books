#include "nr.h"

DP NR::gammq(const DP a, const DP x)
{
	DP gamser,gammcf,gln;

	if (x < 0.0 || a <= 0.0)
		nrerror("Invalid arguments in routine gammq");
	if (x < a+1.0) {
		gser(gamser,a,x,gln);
		return 1.0-gamser;
	} else {
		gcf(gammcf,a,x,gln);
		return gammcf;
	}
}
