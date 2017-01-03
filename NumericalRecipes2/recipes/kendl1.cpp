#include <cmath>
#include "nr.h"
using namespace std;

void NR::kendl1(Vec_I_DP &data1, Vec_I_DP &data2, DP &tau, DP &z, DP &prob)
{
	int is=0,j,k,n2=0,n1=0;
	DP svar,aa,a2,a1;

	int n=data1.size();
	for (j=0;j<n-1;j++) {
		for (k=j+1;k<n;k++) {
			a1=data1[j]-data1[k];
			a2=data2[j]-data2[k];
			aa=a1*a2;
			if (aa != 0.0) {
				++n1;
				++n2;
				aa > 0.0 ? ++is : --is;
			} else {
				if (a1 != 0.0) ++n1;
				if (a2 != 0.0) ++n2;
			}
		}
	}
	tau=is/(sqrt(DP(n1))*sqrt(DP(n2)));
	svar=(4.0*n+10.0)/(9.0*n*(n-1.0));
	z=tau/sqrt(svar);
	prob=erfcc(fabs(z)/1.4142136);
}
