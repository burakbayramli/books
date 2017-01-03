#include "nr.h"

void NR::predic(Vec_I_DP &data, Vec_I_DP &d, Vec_O_DP &future)
{
	int k,j;
	DP sum,discrp;

	int ndata=data.size();
	int m=d.size();
	int nfut=future.size();
	Vec_DP reg(m);
	for (j=0;j<m;j++) reg[j]=data[ndata-1-j];
	for (j=0;j<nfut;j++) {
		discrp=0.0;
		sum=discrp;
		for (k=0;k<m;k++) sum += d[k]*reg[k];
		for (k=m-1;k>=1;k--) reg[k]=reg[k-1];
		future[j]=reg[0]=sum;
	}
}
