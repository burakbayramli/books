#include <cmath>
#include "nr.h"
using namespace std;

void NR::eulsum(DP &sum, const DP term, const int jterm, Vec_IO_DP &wksp)
{
	int j;
	static int nterm;
	DP tmp,dum;

	if (jterm == 0) {
		nterm=1;
		sum=0.5*(wksp[0]=term);
	} else {
		if (nterm+1 > wksp.size()) nrerror("wksp too small in euler");
		tmp=wksp[0];
		wksp[0]=term;
		for (j=1;j<nterm;j++) {
			dum=wksp[j];
			wksp[j]=0.5*(wksp[j-1]+tmp);
			tmp=dum;
		}
		wksp[nterm]=0.5*(wksp[nterm-1]+tmp);
		if (fabs(wksp[nterm]) <= fabs(wksp[nterm-1]))
			sum += (0.5*wksp[nterm++]);
		else
			sum += wksp[nterm];
	}
}
