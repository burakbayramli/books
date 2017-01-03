#include "nr.h"

void NR::beschb(const DP x, DP &gam1, DP &gam2, DP &gampl, DP &gammi)
{
	const int NUSE1=7, NUSE2=8;
	static const DP c1_d[7] = {
		-1.142022680371168e0,6.5165112670737e-3,
		3.087090173086e-4,-3.4706269649e-6,6.9437664e-9,
		3.67795e-11,-1.356e-13};
	static const DP c2_d[8] = {
		1.843740587300905e0,-7.68528408447867e-2,
		1.2719271366546e-3,-4.9717367042e-6,-3.31261198e-8,
		2.423096e-10,-1.702e-13,-1.49e-15};
	DP xx;
	static Vec_DP c1(c1_d,7),c2(c2_d,8);

	xx=8.0*x*x-1.0;
	gam1=chebev(-1.0,1.0,c1,NUSE1,xx);
	gam2=chebev(-1.0,1.0,c2,NUSE2,xx);
	gampl= gam2-x*gam1;
	gammi= gam2+x*gam1;
}
