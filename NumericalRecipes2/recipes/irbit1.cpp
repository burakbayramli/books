#include "nr.h"

int NR::irbit1(unsigned long &iseed)
{
	unsigned long newbit;

	newbit =  ((iseed >> 17) & 1)
		^ ((iseed >> 4) & 1)
		^ ((iseed >> 1) & 1)
		^ (iseed & 1);
	iseed=(iseed << 1) | newbit;
	return int(newbit);
}
