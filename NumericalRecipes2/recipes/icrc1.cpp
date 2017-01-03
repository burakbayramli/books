#include "nr.h"

unsigned short NR::icrc1(const unsigned short crc, const unsigned char onech)
{
	int i;
	unsigned short ans=(crc ^ onech << 8);

	for (i=0;i<8;i++) {
		if (ans & 0x8000)
			ans = (ans <<= 1) ^ 4129;
		else
			ans <<= 1;
	}
	return ans;
}
