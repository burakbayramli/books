#include <string>
#include "nr.h"
using namespace std;

void NR::hufdec(unsigned long &ich, string &code, const unsigned long lcode,
	unsigned long &nb, huffcode &hcode)
{
	unsigned long nc;
	static unsigned char setbit[8]={0x1,0x2,0x4,0x8,0x10,0x20,0x40,0x80};

	int node=hcode.nodemax-1;
	for (;;) {
		nc=nb >> 3;
		if (nc >= lcode) {
			ich=hcode.nch;
			return;
		}
		node=((code[nc] & setbit[7 & nb++]) != 0 ?
			hcode.right[node] : hcode.left[node]);
		if (node < hcode.nch) {
			ich=node;
			return;
		}
	}
}
