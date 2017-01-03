#include "nr.h"

void NR::hufapp(Vec_IO_ULNG &index, Vec_I_ULNG &nprob, const unsigned long n,
	const unsigned long m)
{
	unsigned long i=m,j,k;

	k=index[i];
	while (i < (n >> 1)) {
		if ((j = 2*i+1) < n-1
			&& nprob[index[j]] > nprob[index[j+1]]) j++;
		if (nprob[k] <= nprob[index[j]]) break;
		index[i]=index[j];
		i=j;
	}
	index[i]=k;
}
