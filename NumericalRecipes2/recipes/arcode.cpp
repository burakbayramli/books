#include "nr.h"

namespace {
	inline unsigned long JTRY(const unsigned long j, const unsigned long k,
		const unsigned long m)
	{
		return (unsigned long) (DP(j)*DP(k)/DP (m));
	}
}

void NR::arcode(unsigned long &ich, string &code, unsigned long &lcd,
	const int isign, arithcode &acode)
{
	int j,k;
	unsigned long ihi,ja,jh,jl,m;

	int NWK=acode.ilob.size();
	if (isign == 0) {
		acode.jdif=acode.nrad-1;
		for (j=NWK-1;j>=0;j--) {
			acode.iupb[j]=acode.nrad-1;
			acode.ilob[j]=0;
			acode.nc=j;
			if (acode.jdif > acode.minint) return;
			acode.jdif=(acode.jdif+1)*acode.nrad-1;
		}
		nrerror("NWK too small in arcode.");
	} else {
		if (isign > 0) {
			if (ich > acode.nch) nrerror("bad ich in arcode.");
		} else {
			ja=(unsigned char) code[lcd]-acode.ilob[acode.nc];
			for (j=acode.nc+1;j<NWK;j++) {
				ja *= acode.nrad;
				ja += ((unsigned char) code[lcd+j-acode.nc]-acode.ilob[j]);
			}
			ihi=acode.nch+1;
			ich=0;
			while (ihi-ich > 1) {
				m=(ich+ihi)>>1;
				if (ja >= JTRY(acode.jdif,acode.ncumfq[m],acode.ncum))
					ich=m;
				else ihi=m;
			}
			if (ich == acode.nch) return;
		}
		jh=JTRY(acode.jdif,acode.ncumfq[ich+1],acode.ncum);
		jl=JTRY(acode.jdif,acode.ncumfq[ich],acode.ncum);
		acode.jdif=jh-jl;
		arcsum(acode.ilob,acode.iupb,jh,NWK,acode.nrad,acode.nc);
		arcsum(acode.ilob,acode.ilob,jl,NWK,acode.nrad,acode.nc);
		for (j=acode.nc;j<NWK;j++) {
			if (ich != acode.nch && acode.iupb[j] != acode.ilob[j]) break;
			if (isign > 0) code += (unsigned char)acode.ilob[j];
			lcd++;
		}
		if (j+1 > NWK) return;
		acode.nc=j;
		for(j=0;acode.jdif<acode.minint;j++)
			acode.jdif *= acode.nrad;
		if (j > acode.nc) nrerror("NWK too small in arcode.");
		if (j != 0) {
			for (k=acode.nc;k<NWK;k++) {
				acode.iupb[k-j]=acode.iupb[k];
				acode.ilob[k-j]=acode.ilob[k];
			}
		}
		acode.nc -= j;
		for (k=NWK-j;k<NWK;k++) acode.iupb[k]=acode.ilob[k]=0;
	}
	return;
}
