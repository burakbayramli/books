#include <cmath>
#include "nr.h"
using namespace std;

DP NR::bico(const int n, const int k)
{
	return floor(0.5+exp(factln(n)-factln(k)-factln(n-k)));
}
