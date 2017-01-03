#include <cmath>
#include "nr.h"
using namespace std;

DP NR::expdev(int &idum)
{
	DP dum;

	do
		dum=ran1(idum);
	while (dum == 0.0);
	return -log(dum);
}
