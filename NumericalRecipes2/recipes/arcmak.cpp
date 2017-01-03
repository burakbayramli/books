#include <limits>
#include "nr.h"
using namespace std;

void NR::arcmak(Vec_I_ULNG &nfreq, unsigned long nchh, unsigned long nradd,
	arithcode &acode)
{
	const unsigned long MAXULNG=numeric_limits<unsigned long>::max();
	unsigned long j;

	unsigned long MC=acode.ncumfq.size()-2;
	if (nchh > MC) nrerror("input radix may not exceed MC in arcmak.");
	if (nradd > 256) nrerror("output radix may not exceed 256 in arcmak.");

	acode.minint=MAXULNG/nradd;
	acode.nch=nchh;
	acode.nrad=nradd;
	acode.ncumfq[0]=0;
	for (j=1;j<=acode.nch;j++)
		acode.ncumfq[j]=acode.ncumfq[j-1]+MAX(nfreq[j-1],(unsigned long) 1);
	acode.ncum=acode.ncumfq[acode.nch+1]=acode.ncumfq[acode.nch]+1;
}
