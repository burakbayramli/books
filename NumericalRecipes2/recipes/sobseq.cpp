#include "nr.h"

void NR::sobseq(const int n, Vec_O_DP &x)
{
	const int MAXBIT=30,MAXDIM=6;
	int j,k,l;
	unsigned long i,im,ipp;
	static int mdeg[MAXDIM]={1,2,3,3,4,4};
	static unsigned long in;
	static Vec_ULNG ix(MAXDIM);
	static Vec_ULNG_p iu(MAXBIT);
	static unsigned long ip[MAXDIM]={0,1,1,2,1,4};
	static unsigned long iv[MAXDIM*MAXBIT]=
		{1,1,1,1,1,1,3,1,3,3,1,1,5,7,7,3,3,5,15,11,5,15,13,9};
	static DP fac;

	if (n < 0) {
		for (k=0;k<MAXDIM;k++) ix[k]=0;
		in=0;
		if (iv[0] != 1) return;
		fac=1.0/(1 << MAXBIT);
		for (j=0,k=0;j<MAXBIT;j++,k+=MAXDIM) iu[j] = &iv[k];
		for (k=0;k<MAXDIM;k++) {
			for (j=0;j<mdeg[k];j++) iu[j][k] <<= (MAXBIT-1-j);
			for (j=mdeg[k];j<MAXBIT;j++) {
				ipp=ip[k];
				i=iu[j-mdeg[k]][k];
				i ^= (i >> mdeg[k]);
				for (l=mdeg[k]-1;l>=1;l--) {
					if (ipp & 1) i ^= iu[j-l][k];
					ipp >>= 1;
				}
				iu[j][k]=i;
			}
		}
	} else {
		im=in++;
		for (j=0;j<MAXBIT;j++) {
			if (!(im & 1)) break;
			im >>= 1;
		}
		if (j >= MAXBIT) nrerror("MAXBIT too small in sobseq");
		im=j*MAXDIM;
		for (k=0;k<MIN(n,MAXDIM);k++) {
			ix[k] ^= iv[im+k];
			x[k]=ix[k]*fac;
		}
	}
}
