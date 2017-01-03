#include <cmath>
#include "nr.h"
using namespace std;

DP NR::ellf(const DP phi, const DP ak)
{
	DP s;

	s=sin(phi);
	return s*rf(SQR(cos(phi)),(1.0-s*ak)*(1.0+s*ak),1.0);
}
