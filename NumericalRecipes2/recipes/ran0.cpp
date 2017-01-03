#include "nr.h"

DP NR::ran0(int &idum)
{
	const int IA=16807,IM=2147483647,IQ=127773;
	const int IR=2836,MASK=123459876;
	const DP AM=1.0/DP(IM);
	int k;
	DP ans;

	idum ^= MASK;
	k=idum/IQ;
	idum=IA*(idum-k*IQ)-IR*k;
	if (idum < 0) idum += IM;
	ans=AM*idum;
	idum ^= MASK;
	return ans;
}
