#include <string>
#include "nr.h"
using namespace std;

void NR::hufenc(const unsigned long ich, string &code, unsigned long &nb,
	huffcode &hcode)
{
	int m,n;
	unsigned long k,nc;
	static unsigned long setbit[32]={0x1L,0x2L,0x4L,0x8L,0x10L,0x20L,
		0x40L,0x80L,0x100L,0x200L,0x400L,0x800L,0x1000L,0x2000L,
		0x4000L,0x8000L,0x10000L,0x20000L,0x40000L,0x80000L,0x100000L,
		0x200000L,0x400000L,0x800000L,0x1000000L,0x2000000L,0x4000000L,
		0x8000000L,0x10000000L,0x20000000L,0x40000000L,0x80000000L};

	k=ich;
	if (k >= hcode.nch)
		nrerror("ich out of range in hufenc.");
	for (n=hcode.ncod[k]-1;n >= 0;n--,++nb) {
		nc=nb >> 3;
		if (code.length() < nc+1)
			code.resize(2*(nc+1));
		m=nb & 7;
		if (m == 0) code[nc]=0;
		if ((hcode.icod[k] & setbit[n]) != 0) code[nc] |= setbit[m];
	}
}
