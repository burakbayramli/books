#include <cmath>
#include "nr.h"
using namespace std;

bool NR::metrop(const DP de, const DP t)
{
	static int gljdum=1;

	return de < 0.0 || ran3(gljdum) < exp(-de/t);
}
