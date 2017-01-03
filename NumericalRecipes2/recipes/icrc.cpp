#include "nr.h"

namespace {
	inline unsigned char lobyte(const unsigned short x)
	{
		return (unsigned char)((x) & 0xff);
	}

	inline unsigned char hibyte(const unsigned short x)
	{
		return (unsigned char)((x >> 8) & 0xff);
	}
}

unsigned short NR::icrc(const unsigned short crc, const string &bufptr,
	const short jinit, const int jrev)
{
	static unsigned short icrctb[256],init=0;
	static unsigned char rchr[256];
	unsigned short j,cword=crc;
	static unsigned char it[16]={0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15};

	unsigned long len=bufptr.length();
	if (init == 0) {
		init=1;
		for (j=0;j<256;j++) {
			icrctb[j]=icrc1(j << 8,0);
			rchr[j]=(unsigned char)((it[j & 0xf] << 4) | (it[j >> 4]));
		}
	}
	if (jinit >= 0)
		cword=(jinit | (jinit << 8));
	else if (jrev < 0)
		cword=(rchr[hibyte(cword)] | (rchr[lobyte(cword)] << 8));
	for (j=0;j<len;j++) {
		cword=icrctb[(jrev < 0 ? rchr[(unsigned char) bufptr[j]] :
			(unsigned char) bufptr[j]) ^ hibyte(cword)] ^ (lobyte(cword) << 8);
	}
	return (jrev >= 0 ? cword :
		rchr[hibyte(cword)] | (rchr[lobyte(cword)] << 8));
}
