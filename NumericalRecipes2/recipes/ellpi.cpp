#include <cmath>
#include "nr.h"
using namespace std;

DP NR::ellpi(const DP phi, const DP en, const DP ak)
{
	DP cc,enss,q,s;

	s=sin(phi);
	enss=en*s*s;
	cc=SQR(cos(phi));
	q=(1.0-s*ak)*(1.0+s*ak);
	return s*(rf(cc,q,1.0)-enss*rj(cc,q,1.0,1.0+enss)/3.0);
}
