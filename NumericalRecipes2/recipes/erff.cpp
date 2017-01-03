#include "nr.h"

DP NR::erff(const DP x)
{
	return x < 0.0 ? -gammp(0.5,x*x) : gammp(0.5,x*x);
}
