#include <cstdlib>
#include "nr.h"
using namespace std;

DP NR::ran3(int &idum)
{
	static int inext,inextp;
	static int iff=0;
	const int MBIG=1000000000,MSEED=161803398,MZ=0;
	const DP FAC=(1.0/MBIG);
	static Vec_INT ma(56);
	int i,ii,k,mj,mk;

	if (idum < 0 || iff == 0) {
		iff=1;
		mj=labs(MSEED-labs(idum));
		mj %= MBIG;
		ma[55]=mj;
		mk=1;
		for (i=1;i<=54;i++) {
			ii=(21*i) % 55;
			ma[ii]=mk;
			mk=mj-mk;
			if (mk < int(MZ)) mk += MBIG;
			mj=ma[ii];
		}
		for (k=0;k<4;k++)
			for (i=1;i<=55;i++) {
				ma[i] -= ma[1+(i+30) % 55];
				if (ma[i] < int(MZ)) ma[i] += MBIG;
			}
		inext=0;
		inextp=31;
		idum=1;
	}
	if (++inext == 56) inext=1;
	if (++inextp == 56) inextp=1;
	mj=ma[inext]-ma[inextp];
	if (mj < int(MZ)) mj += MBIG;
	ma[inext]=mj;
	return mj*FAC;
}
