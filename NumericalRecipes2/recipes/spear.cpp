#include <cmath>
#include "nr.h"
using namespace std;

void NR::spear(Vec_I_DP &data1, Vec_I_DP &data2, DP &d, DP &zd, DP &probd,
	DP &rs, DP &probrs)
{
	int j;
	DP vard,t,sg,sf,fac,en3n,en,df,aved;

	int n=data1.size();
	Vec_DP wksp1(n),wksp2(n);
	for (j=0;j<n;j++) {
		wksp1[j]=data1[j];
		wksp2[j]=data2[j];
	}
	sort2(wksp1,wksp2);
	crank(wksp1,sf);
	sort2(wksp2,wksp1);
	crank(wksp2,sg);
	d=0.0;
	for (j=0;j<n;j++)
		d += SQR(wksp1[j]-wksp2[j]);
	en=n;
	en3n=en*en*en-en;
	aved=en3n/6.0-(sf+sg)/12.0;
	fac=(1.0-sf/en3n)*(1.0-sg/en3n);
	vard=((en-1.0)*en*en*SQR(en+1.0)/36.0)*fac;
	zd=(d-aved)/sqrt(vard);
	probd=erfcc(fabs(zd)/1.4142136);
	rs=(1.0-(6.0/en3n)*(d+(sf+sg)/12.0))/sqrt(fac);
	fac=(rs+1.0)*(1.0-rs);
	if (fac > 0.0) {
		t=rs*sqrt((en-2.0)/fac);
		df=en-2.0;
		probrs=betai(0.5*df,0.5,df/(df+t*t));
	} else
		probrs=0.0;
}
