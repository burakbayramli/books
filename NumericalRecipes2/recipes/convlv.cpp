#include "nr.h"

void NR::convlv(Vec_I_DP &data, Vec_I_DP &respns, const int isign,
	Vec_O_DP &ans)
{
	int i,no2;
	DP mag2,tmp;

	int n=data.size();
	int m=respns.size();
	Vec_DP temp(n);
	temp[0]=respns[0];
	for (i=1;i<(m+1)/2;i++) {
		temp[i]=respns[i];
		temp[n-i]=respns[m-i];
	}
	for (i=(m+1)/2;i<n-(m-1)/2;i++)
		temp[i]=0.0;
	for (i=0;i<n;i++)
		ans[i]=data[i];
	realft(ans,1);
	realft(temp,1);
	no2=n>>1;
	if (isign == 1) {
		for (i=2;i<n;i+=2) {
			tmp=ans[i];
			ans[i]=(ans[i]*temp[i]-ans[i+1]*temp[i+1])/no2;
			ans[i+1]=(ans[i+1]*temp[i]+tmp*temp[i+1])/no2;
		}
		ans[0]=ans[0]*temp[0]/no2;
		ans[1]=ans[1]*temp[1]/no2;
	} else if (isign == -1) {
		for (i=2;i<n;i+=2) {
			if ((mag2=SQR(temp[i])+SQR(temp[i+1])) == 0.0)
				nrerror("Deconvolving at response zero in convlv");
			tmp=ans[i];
			ans[i]=(ans[i]*temp[i]+ans[i+1]*temp[i+1])/mag2/no2;
			ans[i+1]=(ans[i+1]*temp[i]-tmp*temp[i+1])/mag2/no2;
		}
		if (temp[0] == 0.0 || temp[1] == 0.0)
			nrerror("Deconvolving at response zero in convlv");
		ans[0]=ans[0]/temp[0]/no2;
		ans[1]=ans[1]/temp[1]/no2;
	} else nrerror("No meaning for isign in convlv");
	realft(ans,-1);
}
