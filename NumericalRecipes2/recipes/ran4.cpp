#include "nr.h"

DP NR::ran4(int &idum)
{
#if defined(vax) || defined(_vax_) || defined(__vax__) || defined(VAX)
	static const unsigned long jflone = 0x00004080;
	static const unsigned long jflmsk = 0xffff007f;
#else
	static const unsigned long jflone = 0x3f800000;
	static const unsigned long jflmsk = 0x007fffff;
#endif
	unsigned long irword,itemp,lword;
	static int idums = 0;

	if (idum < 0) {
		idums = -idum;
		idum=1;
	}
	irword=idum;
	lword=idums;
	psdes(lword,irword);
	itemp=jflone | (jflmsk & irword);
	++idum;
	return (*(float *)&itemp)-1.0;
}
