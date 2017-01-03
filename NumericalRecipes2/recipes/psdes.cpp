#include "nr.h"

void NR::psdes(unsigned long &lword, unsigned long &irword)
{
	const int NITER=4;
	static const unsigned long c1[NITER]={
		0xbaa96887L, 0x1e17d32cL, 0x03bcdc3cL, 0x0f33d1b2L};
	static const unsigned long c2[NITER]={
		0x4b0f3b58L, 0xe874f0c3L, 0x6955c5a6L, 0x55a7ca46L};
	unsigned long i,ia,ib,iswap,itmph=0,itmpl=0;

	for (i=0;i<NITER;i++) {
		ia=(iswap=irword) ^ c1[i];
		itmpl = ia & 0xffff;
		itmph = ia >> 16;
		ib=itmpl*itmpl+ ~(itmph*itmph);
		irword=lword ^ (((ia = (ib >> 16) |
			((ib & 0xffff) << 16)) ^ c2[i])+itmpl*itmph);
		lword=iswap;
	}
}
