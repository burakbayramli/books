#include "nr.h"

int NR::irbit2(unsigned long &iseed)
{
	const unsigned long IB1=1,IB2=2,IB5=16,IB18=131072;
	const unsigned long MASK=IB1+IB2+IB5;

	if (iseed & IB18) {
		iseed=((iseed ^ MASK) << 1) | IB1;
		return 1;
	} else {
		iseed <<= 1;
		return 0;
	}
}
