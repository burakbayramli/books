#include <cmath>
#include "nr.h"
using namespace std;

DP NR::beta(const DP z, const DP w)
{
	return exp(gammln(z)+gammln(w)-gammln(z+w));
}
