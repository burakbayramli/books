#include <cmath>
#include "nr.h"
using namespace std;

extern Vec_DP *xx_p,*yy_p,*sx_p,*sy_p,*ww_p;
extern DP aa,offs;

DP NR::chixy(const DP bang)
{
	const DP BIG=1.0e30;
	int j;
	DP ans,avex=0.0,avey=0.0,sumw=0.0,b;

	Vec_DP &xx=*xx_p, &yy=*yy_p;
	Vec_DP &sx=*sx_p, &sy=*sy_p, &ww=*ww_p;
	int nn=xx.size();
	b=tan(bang);
	for (j=0;j<nn;j++) {
		ww[j] = SQR(b*sx[j])+SQR(sy[j]);
		sumw += (ww[j]=(ww[j] < 1.0/BIG ? BIG : 1.0/ww[j]));
		avex += ww[j]*xx[j];
		avey += ww[j]*yy[j];
	}
	avex /= sumw;
	avey /= sumw;
	aa=avey-b*avex;
	for (ans = -offs,j=0;j<nn;j++)
		ans += ww[j]*SQR(yy[j]-aa-b*xx[j]);
	return ans;
}
