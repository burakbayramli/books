#include <cmath>
#include "nr.h"
using namespace std;

void NR::ttest(Vec_I_DP &data1, Vec_I_DP &data2, DP &t, DP &prob)
{
	DP var1,var2,svar,df,ave1,ave2;

	int n1=data1.size();
	int n2=data2.size();
	avevar(data1,ave1,var1);
	avevar(data2,ave2,var2);
	df=n1+n2-2;
	svar=((n1-1)*var1+(n2-1)*var2)/df;
	t=(ave1-ave2)/sqrt(svar*(1.0/n1+1.0/n2));
	prob=betai(0.5*df,0.5,df/(df+t*t));
}
