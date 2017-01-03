#include <cmath>
#include "nr.h"
using namespace std;

DP NR::pythag(const DP a, const DP b)
{
	DP absa,absb;

	absa=fabs(a);
	absb=fabs(b);
	if (absa > absb) return absa*sqrt(1.0+SQR(absb/absa));
	else return (absb == 0.0 ? 0.0 : absb*sqrt(1.0+SQR(absa/absb)));
}
