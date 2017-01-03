#include <cmath>
#include "nr.h"
using namespace std;

void NR::tptest(Vec_I_DP &data1, Vec_I_DP &data2, DP &t, DP &prob)
{
	int j;
	DP var1,var2,ave1,ave2,sd,df,cov=0.0;

	int n=data1.size();
	avevar(data1,ave1,var1);
	avevar(data2,ave2,var2);
	for (j=0;j<n;j++)
		cov += (data1[j]-ave1)*(data2[j]-ave2);
	cov /= df=n-1;
	sd=sqrt((var1+var2-2.0*cov)/n);
	t=(ave1-ave2)/sd;
	prob=betai(0.5*df,0.5,df/(df+t*t));
}
