#include "nr.h"

void NR::hufmak(Vec_I_ULNG &nfreq, const unsigned long nchin,
	unsigned long &ilong, unsigned long &nlong, huffcode &hcode)
{
	int ibit,j,node;
	unsigned long k,n,nused;
	static unsigned long setbit[32]={0x1L,0x2L,0x4L,0x8L,0x10L,0x20L,
		0x40L,0x80L,0x100L,0x200L,0x400L,0x800L,0x1000L,0x2000L,
		0x4000L,0x8000L,0x10000L,0x20000L,0x40000L,0x80000L,0x100000L,
		0x200000L,0x400000L,0x800000L,0x1000000L,0x2000000L,0x4000000L,
		0x8000000L,0x10000000L,0x20000000L,0x40000000L,0x80000000L};

	hcode.nch=nchin;
	Vec_ULNG index(2*hcode.nch-1);
	Vec_ULNG nprob(2*hcode.nch-1);
	Vec_INT up(2*hcode.nch-1);
	for (nused=0,j=0;j<hcode.nch;j++) {
		nprob[j]=nfreq[j];
		hcode.icod[j]=hcode.ncod[j]=0;
		if (nfreq[j] != 0) index[nused++]=j;
	}
	for (j=nused-1;j>=0;j--)
		hufapp(index,nprob,nused,j);
	k=hcode.nch;
	while (nused > 1) {
		node=index[0];
		index[0]=index[(nused--)-1];
		hufapp(index,nprob,nused,0);
		nprob[k]=nprob[index[0]]+nprob[node];
		hcode.left[k]=node;
		hcode.right[k++]=index[0];
		up[index[0]] = -int(k);
		index[0]=k-1;
		up[node]=k;
		hufapp(index,nprob,nused,0);
	}
	up[(hcode.nodemax=k)-1]=0;
	for (j=0;j<hcode.nch;j++) {
		if (nprob[j] != 0) {
			for (n=0,ibit=0,node=up[j];node;node=up[node-1],ibit++) {
				if (node < 0) {
					n |= setbit[ibit];
					node = -node;
				}
			}
			hcode.icod[j]=n;
			hcode.ncod[j]=ibit;
		}
	}
	nlong=0;
	for (j=0;j<hcode.nch;j++) {
		if (hcode.ncod[j] > nlong) {
			nlong=hcode.ncod[j];
			ilong=j;
		}
	}
}
