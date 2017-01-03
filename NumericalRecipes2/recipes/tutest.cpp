#include <cmath>
#include "nr.h"
using namespace std;

void NR::tutest(Vec_I_DP &data1, Vec_I_DP &data2, DP &t, DP &prob)
{
	DP var1,var2,df,ave1,ave2;

	int n1=data1.size();
	int n2=data2.size();
	avevar(data1,ave1,var1);
	avevar(data2,ave2,var2);
	t=(ave1-ave2)/sqrt(var1/n1+var2/n2);
	df=SQR(var1/n1+var2/n2)/(SQR(var1/n1)/(n1-1)+SQR(var2/n2)/(n2-1));
	prob=betai(0.5*df,0.5,df/(df+SQR(t)));
}
