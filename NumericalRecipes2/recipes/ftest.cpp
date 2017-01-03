#include "nr.h"

void NR::ftest(Vec_I_DP &data1, Vec_I_DP &data2, DP &f, DP &prob)
{
	DP var1,var2,ave1,ave2,df1,df2;

	int n1=data1.size();
	int n2=data2.size();
	avevar(data1,ave1,var1);
	avevar(data2,ave2,var2);
	if (var1 > var2) {
		f=var1/var2;
		df1=n1-1;
		df2=n2-1;
	} else {
		f=var2/var1;
		df1=n2-1;
		df2=n1-1;
	}
	prob = 2.0*betai(0.5*df2,0.5*df1,df2/(df2+df1*f));
	if (prob > 1.0) prob=2.0-prob;
}
