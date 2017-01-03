#include "nr.h"

void NR::correl(Vec_I_DP &data1, Vec_I_DP &data2, Vec_O_DP &ans)
{
	int no2,i;
	DP tmp;

	int n=data1.size();
	Vec_DP temp(n);
	for (i=0;i<n;i++) {
		ans[i]=data1[i];
		temp[i]=data2[i];
	}
	realft(ans,1);
	realft(temp,1);
	no2=n>>1;
	for (i=2;i<n;i+=2) {
		tmp=ans[i];
		ans[i]=(ans[i]*temp[i]+ans[i+1]*temp[i+1])/no2;
		ans[i+1]=(ans[i+1]*temp[i]-tmp*temp[i+1])/no2;
	}
	ans[0]=ans[0]*temp[0]/no2;
	ans[1]=ans[1]*temp[1]/no2;
	realft(ans,-1);
}
